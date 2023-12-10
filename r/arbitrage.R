library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)
library(DescTools)

# Input:
#   - panel: panel data
#   - spec: linear regression model
# Assume month as the time series index
fm <- function(panel, spec) {
  panel %>%
    nest(.by=month, .key="cross_section") %>%
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
    mutate(t_stat = estimate / newey_west_se) %>%
    select(factor, estimate, t_stat)
}

# Construct the panel

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(db, "crsp_monthly") |>
  collect()

amihud_illiq <- tbl(db, "amihud_illiq") |>
  collect()

crsp_io <- tbl(db, "crsp_io") |>
  collect()

crsp_disp <- tbl(db, "crsp_disp") |>
  collect()

size_bm_mom <- tbl(db, "factors_size_bm_mom") |>
  collect()

t <- crsp_monthly |>
  select(permno, month, ret) |>
  inner_join(amihud_illiq, by = c("permno", "month")) |>
  inner_join(crsp_io, by = c("permno", "month")) |>
  inner_join(crsp_disp, by = c("permno", "month")) |>
  inner_join(size_bm_mom, by = c("permno", "month"))

ret_lead <- panel |>
  mutate(month = month %m-% months(1)) |>
  select(permno, month, ret_lead=ret)

panel <- t |>
  left_join(ret_lead, by = c("permno", "month")) |>
  filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))

fm(panel, ret_lead ~ amihud_illiq + inst_own + disp + size + bm + mom)
