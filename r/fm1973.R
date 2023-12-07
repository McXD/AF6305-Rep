library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)
library(DescTools)

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, month, ret_excess) |>
  collect()

factors <- tbl(db, "factors_size_bm_mom") |>
  select(permno, month, size, bm, mom) |>
  collect()

betas <- tbl(db, "betas_ff3_1m") |>
  select(permno, month, ivol = res_std) |>
  collect()

data <- crsp_monthly |>
  inner_join(factors, by = c("permno", "month")) |>
  inner_join(betas, by = c("permno", "month")) |>
  inner_join(crsp_monthly |> select(permno, month, ret_lead = ret_excess) |> mutate(month = month %m-% months(1)), by = c("permno", "month")) |>
  drop_na() |>
  # winsorize at 0.5% each month
  group_by(month) |>
  mutate(
    size = Winsorize(size, probs = c(0.005, 0.995)),
    bm = Winsorize(bm, probs = c(0.005, 0.995)),
    mom = Winsorize(mom, probs = c(0.005, 0.995)),
    ivol = Winsorize(ivol, probs = c(0.005, 0.995))
  ) |>
  ungroup() |>
  nest(cross_section = c(permno, ret_lead, size, bm, mom, ret_excess, ivol))

# Define the function
fm <- function(data, independent_vars) {
  # Create a formula string for the regression model
  formula_str <- paste('ret_lead', "~", paste(independent_vars, collapse = " + "))
  
  data %>%
    mutate(estimates = map(
      cross_section,
      ~ tidy(lm(as.formula(formula_str), data = .x))
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
    mutate(t_stat = estimate / newey_west_se) %>%
    select(factor, estimate, t_stat)
}

fm(data, c("bm", "size"))
