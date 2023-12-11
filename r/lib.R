# This script contains all the utilities that are used across the project
# To be sourced in almost every file.

# Packages ----------------------------------------------------------------
library(dbplyr)
library(tidyverse)
library(frenchdata)
library(RSQLite)
library(RPostgres)
library(progress)
library(moments)
library(slider)
library(pbapply)
library(sandwich)
library(DescTools)
library(lmtest)
library(broom)


# Variables ---------------------------------------------------------------

start_date <- ymd("1996-01-01")
end_date <- ymd("2020-12-31")

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

# Functions ---------------------------------------------------------------

# Return a WRDS connection
# Use a function instead of a variable since a connection takes a while
# and we don't want the connection when not needed.
# Call dbDisconnect(wrds) when finished
wrds_connect <- function() {
  dbConnect(
    Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USERNAME"),
    password = Sys.getenv("WRDS_PASSWORD")
  )
}

# Calculate summary statistics of a panel
# df: the panel data
# cols: the columns to calculate summary statistics
# return: a data frame with summary statistics
# Assume time index is 'month'
sum_stats <- function(df, cols) {
  results <- data.frame()
  
  for (col in cols) {
    # remove NA, NaN, Inf for this column
    df_na <- df %>%
      filter(!is.na(!!rlang::sym(col))) %>%
      filter(!is.nan(!!rlang::sym(col))) %>%
      filter(!is.infinite(!!rlang::sym(col)))
    
    result <- df_na |>
      group_by(date) |> # for a cross-section
      summarise(
        mean = mean(!!rlang::sym(col)),
        sd = sd(!!rlang::sym(col)),
        skew = skewness(!!rlang::sym(col)),
        kurtosis = kurtosis(!!rlang::sym(col)),
        min = min(!!rlang::sym(col)),
        `5%` = quantile(!!rlang::sym(col), 0.05),
        `25%` = quantile(!!rlang::sym(col), 0.25),
        median = median(!!rlang::sym(col)),
        `75%` = quantile(!!rlang::sym(col), 0.75),
        `95%` = quantile(!!rlang::sym(col), 0.95),
        max = max(!!rlang::sym(col)),
        n = sum(!is.na(!!rlang::sym(col)))
      ) |>
      summarise(
        mean = mean(mean),
        sd = mean(sd),
        skew = mean(skew),
        kurtosis = mean(kurtosis),
        min = mean(min),
        `5%` = mean(`5%`),
        `25%` = mean(`25%`),
        median = mean(median),
        `75%` = mean(`75%`),
        `95%` = mean(`95%`),
        max = mean(max),
        n = floor(mean(n))
      )
    
    result$var <- col # Add the column name to the result
    result <- result |> select(var, everything()) # Move the column name to the front
    results <- rbind(results, result) # Append the result to the results data frame
  }
  
  return(results)
}

# Calculate the correlation among variables
# data: panel data
# cols: the columns to calculate correlation
# Assume time index is 'date'
cor_stats <- function(data, cols) {
  # Split the data by month
  data_by_month <- data %>% mutate(month = month(date)) %>% 
    split(.$month)
  
  # Initialize empty list to store correlation matrices
  cor_matrices <- list()
  
  # Loop over each month
  for(i in 1:length(data_by_month)) {
    # Select relevant columns and drop NA values
    data_month <- data_by_month[[i]] %>% select(all_of(cols)) %>% na.omit()
    
    # Calculate Spearman and Pearson correlations
    spearman <- cor(data_month, method = "spearman")
    pearson <- cor(data_month, method = "pearson")
    
    # Combine into one matrix, with Spearman above diagonal and Pearson below
    cor_matrix <- spearman
    cor_matrix[lower.tri(cor_matrix)] <- pearson[lower.tri(pearson)]
    
    # Store in list
    cor_matrices[[i]] <- cor_matrix
  }
  
  # Calculate time-series average of the matrices
  avg_cor_matrix <- Reduce("+", cor_matrices) / length(cor_matrices)
  
  return(avg_cor_matrix)
}

# Fama-MacBeth regression
# panel: panel data
# ts_index: the time series index column
# spec: linear regression model
# return: a data frame with estimate and t-statistics for each factor
fm <- function(panel, ts_index, spec) {
  panel %>%
    nest(.by= {{ts_index}}, .key="cross_section") %>%
    mutate(estimates = map(
      cross_section,
      ~ tidy(lm(spec, data = .x))
    )) %>%
    unnest(estimates) %>%
    select(month, factor = term, estimate) %>%
    nest(time_series = c(month, estimate)) %>%
    mutate(
      model = map(time_series, ~ lm(estimate ~ 1, .)),
      result = map(model, tidy)
    ) %>%
    mutate(newey_west_se = map_dbl(model, ~ sqrt(NeweyWest(.)))) %>%
    unnest(result) %>%
    mutate(nw_t_stat = estimate / newey_west_se) %>%
    select(factor, estimate, nw_t_stat)
}