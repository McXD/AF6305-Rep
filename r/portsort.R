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
  inner_join(factors, by = c("permno", "month")) |>
  drop_na()

# Add lag variables
data <- data |>
  left_join(
    data |> select(permno, month, size_lag = size, bm_lag = bm, mom_lag = mom, mktcap_lag = mktcap) |> mutate(month = month %m+% months(1)),
    by = c("permno", "month")
  )

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

single_sort <- function(
    data, # panel data
    sort_variable,
    n_portfolios,
    sort_data = data,
    probs = seq(0, 1, length.out = n_portfolios + 1)
){
  portfolios <- data |>
    drop_na() |>
    group_by(month) |>
    mutate(
      portfolio = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable }},
        n_portfolios = n_portfolios,
        sort_data = pick(everything()) %>% filter(exchcd %in% c(1, 31)),
        probs = probs
      ) %>%
        as.factor()
    ) |>
    group_by(portfolio, month) |> # cross-sectional 
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap_lag),
      .groups = "drop"
    ) |>
    group_by(portfolio) |> # time-series average
    summarize(
      avg_ew_ret = mean(ew_ret),
      ew_tstat = {
        lm_model <- lm(ew_ret ~ 1)
        summary_model <- summary(lm_model, vcov = NeweyWest(lm_model))
        summary_model$coefficients["(Intercept)", "t value"]
      },
      avg_vw_ret = mean(vw_ret),
      vw_tstat = {
        lm_model <- lm(vw_ret ~ 1)
        summary_model <- summary(lm_model, vcov = NeweyWest(lm_model))
        summary_model$coefficients["(Intercept)", "t value"]
      },
      .groups = "drop"
    )
  
  return (portfolios)
}

indepedent_double_sort <- function(
    data, # panel data
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    sort_data = data
){
  portfolios <- data |>
    drop_na() |>
    group_by(month) |>
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
    group_by(portfolio_1, portfolio_2, month) |> # cross-sectional 
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap_lag),
      .groups = "drop"
    ) |>
    group_by(portfolio_1, portfolio_2) |> # time-series average
    summarize(
      avg_ew_ret = mean(ew_ret),
      ew_tstat = {
        lm_model <- lm(ew_ret ~ 1)
        summary_model <- summary(lm_model, vcov = NeweyWest(lm_model))
        summary_model$coefficients["(Intercept)", "t value"]
      },
      avg_vw_ret = mean(vw_ret),
      vw_tstat = {
        lm_model <- lm(vw_ret ~ 1)
        summary_model <- summary(lm_model, vcov = NeweyWest(lm_model))
        summary_model$coefficients["(Intercept)", "t value"]
      },
      .groups = "drop"
    )
  
  return (portfolios)
}

dependent_double_sort <- function(
    data, # panel data
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    sort_data = data
){
  portfolios <- data |>
    drop_na() |>
    group_by(month) |>
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
    group_by(portfolio_1, portfolio_2, month) |> # cross-sectional 
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap_lag),
      .groups = "drop"
    ) |>
    group_by(portfolio_1, portfolio_2) |> # time-series average
    summarize(
      avg_ew_ret = mean(ew_ret),
      ew_tstat = {
        lm_model <- lm(ew_ret ~ 1)
        summary_model <- summary(lm_model, vcov = NeweyWest(lm_model))
        summary_model$coefficients["(Intercept)", "t value"]
      },
      avg_vw_ret = mean(vw_ret),
      vw_tstat = {
        lm_model <- lm(vw_ret ~ 1)
        summary_model <- summary(lm_model, vcov = NeweyWest(lm_model))
        summary_model$coefficients["(Intercept)", "t value"]
      },
      .groups = "drop"
    )
  
  return (portfolios)
}

# Single Sort
single_sort(data, size_lag, 10)
single_sort(data, bm_lag, 10)

mom_data <- data |>
  left_join(
    data |>
      filter(exchcd %in% c(1, 31)) |>
      group_by(month) |>
      summarize(
        size_10th = quantile(size_lag, probs = 0.1, na.rm = TRUE),
        .groups = "drop"
      ), # NYSE breakpoints
    by = "month"
  ) |>
  filter(size >= size_10th) |>
  filter(prc > 5) |>
  select(-size_10th)
single_sort(mom_data, mom_lag, 5)

# Independent Double Sort
indepedent_double_sort(data, size_lag, bm_lag, n_portfolios_1 =5, n_portfolios_2 = 5)
dependent_double_sort(data, size_lag, bm_lag, n_portfolios_1 =5, n_portfolios_2 = 5)

# Size --------------------------------------------------------------------

size_portfolios <- data |>
  drop_na() |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      sorting_variable = size_lag,
      n_portfolios = 10,
      sort_data = cur_data() %>% filter(exchcd %in% c(1, 31))
    ) %>%
      as.factor()
  ) |>
  group_by(portfolio, month) |>
  summarize(
    ret_excess = mean(ret_excess),
    .groups = "drop"
  ) |>
  left_join(
    factors_ff3_monthly,
    by = "month"
  )

size_portfolios_summary <- size_portfolios |>
  nest(data = c(month, ret_excess, mkt_excess)) |>
  mutate(estimates = map(
    data, ~ tidy(lm(ret_excess ~ 1 + mkt_excess, data = .x))
  )) |>
  unnest(estimates) |>
  select(portfolio, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  rename(alpha = `(Intercept)`, beta = mkt_excess) |>
  left_join(
    size_portfolios |>
      group_by(portfolio) |>
      summarize(
        ret_excess = mean(ret_excess),
        .groups = "drop"
      ),
    by = "portfolio"
  )
