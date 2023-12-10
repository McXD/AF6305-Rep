library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

# Load Data -------------------------------------------------------------------

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, month, mktcap, ret_excess, exchcd, prc) |>
  collect()

factors <- tbl(db, "factors_size_bm_mom") |>
  select(permno, month, size, bm, mom) |>
  collect()

factors_ff3_monthly <- tbl(db, "factors_ff3_monthly") |>
  select(month, mkt_excess) |>
  collect()

data <- crsp_monthly |>
  inner_join(factors, by = c("permno", "month"))

# Lead the return
data <- data |>
  left_join(
    data |> select(permno, month, ret_excess_lead = ret_excess) |> mutate(month = month %m-% months(1)),
    by = c("permno", "month")
  ) |>
  select(-ret_excess) |>
  rename(ret_excess = ret_excess_lead) |>
  drop_na()

# Functions ---------------------------------------------------------------

# Univariate sort
# Return assigned portfolios
# data is cross-sectional
assign_portfolio <- function(
    data,
    sort_variable,
    n_portfolios,
    sort_data = data,  # New argument with default value as 'data'
    probs = seq(0, 1, length.out = n_portfolios + 1)  # New argument with default probability sequence
) {
  # Compute breakpoints using 'sort_data'
  breakpoints <- sort_data |>
    pull({{ sort_variable }}) |>
    quantile(
      probs = probs,  # Use the provided 'prob' argument
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |> pull({{ sort_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  
  return(assigned_portfolios)
}

# Sort a cross-sectional data set into portfolios and calculate portfolio returns
# Return: data frame with portfolio returns (ew and vw)
single_sort <- function(
    cs,
    sort_variable,
    n_portfolios,
    sort_data = cs
){
  portfolios <- cs |>
    drop_na() |>
    mutate(
      portfolio = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable }},
        n_portfolios = n_portfolios,
        sort_data = pick(everything()) %>% filter(exchcd %in% c(1, 31))
      ) %>%
        as.factor()
    ) |>
    group_by(portfolio) |>
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap),
      .groups = "drop"
    )
  
  return (portfolios)
}

newey_west_t_stat <- function(ts, colname) {
  model <- lm(ts[[colname]] ~ 1)  
  summary_model <- summary(model, vcov = NeweyWest(model))
  summary_model$coefficients["(Intercept)", "t value"]
}

single_sort_panel <- function(
    panel, # panel data
    sort_variable,
    n_portfolios,
    sort_data = data
) {
  panel |>
    nest(.by = month, .key = "cs") |>
    mutate(
      portfolio_ret = map(cs, single_sort, {{ sort_variable }}, n_portfolios, sort_data)
    ) |>
    unnest(portfolio_ret) |>
    nest(.by = portfolio, .key = "ts") |>
    mutate(
      avg_ew_ret = map_dbl(ts, ~ mean(.x$ew_ret)),
      var_ew_ret = map_dbl(ts, ~ var(.x$ew_ret)),
      ew_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "ew_ret")),
      avg_vw_ret = map_dbl(ts, ~ mean(.x$vw_ret)),
      var_vw_ret = map_dbl(ts, ~ var(.x$vw_ret)),
      vw_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "vw_ret"))
    ) |>
    select(-ts)
}

single_sort_panel(data, size, 10)
single_sort_panel(data, bm, 10)
single_sort_panel(data, mom, 10)

independent_double_sort <- function(
    cs,
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    sort_data = data
){
  portfolios <- cs |>
    drop_na() |>
    mutate(
      portfolio_1 = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable_1 }},
        n_portfolios = n_portfolios_1,
        sort_data = pick(everything()) %>% filter(exchcd %in% c(1, 31))
      ) %>%
        as.factor(),
      portfolio_2 = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable_2 }},
        n_portfolios = n_portfolios_2,
        sort_data = pick(everything()) %>% filter(exchcd %in% c(1, 31))
      ) %>%
        as.factor()
    ) |>
    group_by(portfolio_1, portfolio_2) |>
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap),
      .groups = "drop"
    )
  
  return (portfolios)
}

independent_double_sort_panel <- function(
    panel, # panel data
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    sort_data = data
) {
  panel |>
    nest(.by = month, .key = "cs") |>
    mutate(
      portfolio_ret = map(cs, independent_double_sort, {{ sort_variable_1 }}, {{ sort_variable_2 }}, n_portfolios_1, n_portfolios_2, sort_data)
    ) |>
    unnest(portfolio_ret) |>
    nest(.by = c(portfolio_1, portfolio_2), .key = "ts") |>
    mutate(
      avg_ew_ret = map_dbl(ts, ~ mean(.x$ew_ret)),
      ew_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "ew_ret")),
      avg_vw_ret = map_dbl(ts, ~ mean(.x$vw_ret)),
      vw_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "vw_ret"))
    ) |>
    select(-ts)
}

dependent_double_sort <- function(
    cs,
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    sort_data = data
){
  portfolios <- cs |>
    drop_na() |>
    mutate(
      portfolio_1 = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable_1 }},
        n_portfolios = n_portfolios_1,
        sort_data = pick(everything()) %>% filter(exchcd %in% c(1, 31))
      ) %>%
        as.factor()
    ) |>
    group_by(portfolio_1) |>
    mutate(
      portfolio_2 = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable_2 }},
        n_portfolios = n_portfolios_2,
        sort_data = pick(everything()) %>% filter(exchcd %in% c(1, 31))
      ) %>%
        as.factor()
    ) |>
    ungroup() |>
    group_by(portfolio_1, portfolio_2) |>
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap),
      .groups = "drop"
    )
  
  return (portfolios)
}

dependent_double_sort_panel <- function(
    panel, # panel data
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    sort_data = data
) {
  panel |>
    nest(.by = month, .key = "cs") |>
    mutate(
      portfolio_ret = map(cs, dependent_double_sort, {{ sort_variable_1 }}, {{ sort_variable_2 }}, n_portfolios_1, n_portfolios_2, sort_data)
    ) |>
    unnest(portfolio_ret) |>
    nest(.by = c(portfolio_1, portfolio_2), .key = "ts") |>
    mutate(
      avg_ew_ret = map_dbl(ts, ~ mean(.x$ew_ret)),
      ew_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "ew_ret")),
      avg_vw_ret = map_dbl(ts, ~ mean(.x$vw_ret)),
      vw_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "vw_ret"))
    ) |>
    select(-ts)
}

independent_double_sort_panel(data, size, bm, n_portfolios_1 =5, n_portfolios_2 = 5)
dependent_double_sort_panel(data, size, bm, n_portfolios_1 =5, n_portfolios_2 = 5)

mom_data <- data |>
  left_join(
    data |>
      filter(exchcd %in% c(1, 31)) |>
      group_by(month) |>
      summarize(
        size_10th = quantile(size, probs = 0.1, na.rm = TRUE),
        .groups = "drop"
      ), # NYSE breakpoints
    by = "month"
  ) |>
  filter(size >= size_10th) |>
  filter(prc > 5) |>
  select(-size_10th)

single_sort_panel(mom_data, mom, 10)
independent_double_sort_panel(mom_data, size, mom, 5, 5)
dependent_double_sort_panel(mom_data, size, mom, 5, 5)
