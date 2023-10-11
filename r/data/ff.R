source("r/data/common.R")

# SQLite ------------------------------------------------------------------
db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

# FF [Monthly] ------------------------------------------------------------
factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")

factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
  mutate(
    month = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(month >= start_date & month <= end_date)

dbWriteTable(db, "factors_ff3_monthly", factors_ff3_monthly, overwrite = TRUE)

# FF [Daily] --------------------------------------------------------------
factors_ff3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")

factors_ff3_daily <- factors_ff3_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date & date <= end_date)

dbWriteTable(db, "factors_ff3_daily", factors_ff3_daily, overwrite = TRUE)
