# This script test linking tr_13f with crsp_monthly

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

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, cusip, date, month, ret, ret_excess) |>
  collect()

tr_13f <- tbl(db, "tr_13f") |> collect()

data <- crsp_monthly |>
  left_join(tr_13f, by = c("cusip", "month")) |>
  select(permno, month, ret, ret_excess, inst_own)

# roll forward the quarterly value
t <- data |>
  mutate(year = year(month), quarter = quarter(month)) |>
  group_by(permno, year, quarter) |>
  fill(inst_own, .direction = "up") |>
  ungroup() |>
  select(-year, -quarter)

# check proportion of missing values
t |>
  group_by(permno) |>
  summarize(
    prop_missing = mean(is.na(inst_own))
  ) |>
  summarize(
    prop_missing = mean(prop_missing)
  )

t <- t |> drop_na()

# Save
dbWriteTable(db, "crsp_io", t, overwrite = TRUE)
