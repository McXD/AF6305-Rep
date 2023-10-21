library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, month, mktcap, ret_excess, exchcd) |>
  collect()

factor_size <- tbl(db, "factor_size") |>
  collect()

factors_ff3_monthly <- tbl(db, "factors_ff3_monthly") |>
  select(month, mkt_excess) |>
  collect()

data <- crsp_monthly |>
  inner_join(factor_size, by = c("permno", "month")) |>
  drop_na()

# Add lag variables
data <- data |>
  left_join(
    data |> select(permno, month, size_lag = size, size_ff_lag = size_ff) |> mutate(month = month %m+% months(1)),
    by = c("permno", "month")
  )

assign_portfolio <- function(
    data,
    sorting_variable,
    n_portfolios) {
  # Compute breakpoints using NYSE breakpoints
  breakpoints <- data |>
    filter(exchcd %in% c(1, 31)) |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |> pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)
  
  return(assigned_portfolios)
}

data_port <- data |>
  drop_na() |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = size_lag,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) |>
  ungroup()

# Portfolio characteristics
data_port |>
  drop_na() |>
  group_by(month, portfolio) |>
  summarize(
    n = n(),
    mktcap = mean(mktcap),
    .groups = "drop"
  ) |>
  group_by(portfolio) |>
  summarize(
    n = mean(n),
    mktcap = mean(mktcap),
    .groups = "drop"
  )

size_portfolios <- data |>
  drop_na() |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = size_ff_lag,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, month) |>
  summarize(
    ret_excess = weighted.mean(ret_excess, size_ff_lag),
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
