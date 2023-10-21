library(tidyverse)
library(RSQLite)
library(slider)


# Load Data ---------------------------------------------------------------

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, month, mktcap, ret, gvkey) |>
  collect()

compustat <- tbl(db, "compustat") |>
  select(gvkey, year, be) |>
  collect()

# Annual Size -------------------------------------------------------------

# `Size` is `mktcap` at June of year t
# `ME` (market equity) is `mktcap` at December of year t-1
factors <- crsp_monthly |>
  group_by(permno, year = year(month)) |>
  mutate(size = if_else(month(month) == 6, mktcap, NA_real_)) |> # size at June (t)
  mutate(me = if_else(month(month) == 12, mktcap, NA_real_)) |> # me at December (t)
  summarise(
    size = last(size, na_rm = TRUE),
    me = last(me, na_rm = TRUE),
    gvkey = last(gvkey, na_rm = TRUE)
  )

# Shift `me` to t-1
factors <- factors |>
  left_join(
    factors |> select(permno, year, me_lagged = me) |> mutate(year = year + 1),
    by = c("permno", "year")
  ) |>
  select(-me) |>
  rename(me = me_lagged)

# Annual BM ---------------------------------------------------------------

factors <- factors |>
  left_join(
    compustat |> mutate(year = year + 1),
    by = c("gvkey", "year")
  ) |>
  mutate(bm = be / me) |>
  select(-be, -me, -gvkey)

# Monthly MOM -------------------------------------------------------------

cumret <- function(data, min_obs = 5) {
  if (nrow(data) < min_obs) {
    return(NA_real_)
  } else {
    return(prod(1 + data$ret) - 1)
  }
}

roll_mom_estimation <- function(data, months, min_obs) {
  slide_period_dbl(
    .x = data,
    .i = data$month, # index
    .period = "month", # aggregation is applied to each month
    .f = ~ cumret(., min_obs),
    .before = months - 1,
    .complete = FALSE # ignore incomplete periods
  )
}

# Sanity Check
examples <- tribble(
  ~permno, ~company,
  14593, "Apple",
  10107, "Microsoft",
  93436, "Tesla",
  17778, "Berkshire Hathaway"
)

t <- crsp_monthly %>%
  inner_join(examples, by = "permno") %>%
  group_by(permno) %>%
  arrange(month) %>%
  group_modify(~ mutate(., mom = roll_mom_estimation(., months = 6, min_obs = 5))) %>%
  ungroup()

# Calculate MOM
mom_monthly <- crsp_monthly %>%
  group_by(permno) %>%
  arrange(month) %>%
  group_modify(~ mutate(., mom = roll_mom_estimation(., months = 6, min_obs = 5))) %>%
  ungroup()

# Shift `mom` to t-1
mom_monthly <- mom_monthly |>
  left_join(
    mom_monthly |>
      group_by(permno) |>
      mutate(month = month %m+% months(1)) |>
      ungroup() |>
      select(permno, month, mom_lagged = mom),
    by = c("permno", "month")
  ) |>
  select(-mom) |>
  rename(mom = mom_lagged)

# Merge all factors -------------------------------------------------------

all_factors <- mom_monthly |>
  mutate(year = year(month)) |>
  left_join(
    factors,
    by = c("permno", "year")
  ) |>
  select(permno, month, mom, size, bm)

# Shift size by 5 months
all_factors <- all_factors |>
  left_join(
    all_factors |>
      group_by(permno) |>
      mutate(month = month %m+% months(5)) |>
      ungroup() |>
      select(permno, month, size_lagged = size),
    by = c("permno", "month")
  ) |>
  select(-size) |>
  rename(size = size_lagged)

# Save --------------------------------------------------------------------

dbWriteTable(
  db,
  "factors_size_bm_mom",
  all_factors,
  overwrite = TRUE
)
