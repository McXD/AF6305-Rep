library(tidyverse)
library(slider)
library(furrr)
library(RSQLite)
library(progressr)
library(knitr)
library(kableExtra)
library(stargazer)

# Load data ---------------------------------------------------------------

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_daily <- tbl(db, "crsp_daily") |>
  select(permno, date, month, ret, vol) |>
  collect() |>
  drop_na()

# Adjust volumes for Nasdaq stocks
# Prior to 2001-02-01, divide by 2
# 2001-02-01 to 2001-12-31, divide by 1.8
# 2002-01-01 to 2004-12-31, divide by 1.6
# After 2005-01-01, divide by 1 (no adjustment)
crsp_daily <- crsp_daily |>
  mutate(
    vol_adj = case_when(
      exchcd %in% c(1, 31) & date < as.Date("2001-02-01") ~ 2,
      exchcd %in% c(1, 31) & date >= as.Date("2001-02-01") & date < as.Date("2001-12-31") ~ 1.8,
      exchcd %in% c(1, 31) & date >= as.Date("2002-01-01") & date < as.Date("2004-12-31") ~ 1.6,
      TRUE ~ 1
    ),
    vol = vol / vol_adj
  )

# amihub_illiq = ret/vol
# take monthly average
amihud_illiq <- crsp_daily |>
  filter(vol != 0) |>
  mutate(amihud_illiq = abs(ret) / vol) |>
  group_by(permno, month) |>
  summarize(
    amihud_illiq = mean(amihud_illiq, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(permno, month, amihud_illiq)

# Save
dbWriteTable(db, "amihud_illiq", amihud_illiq, overwrite = TRUE)

