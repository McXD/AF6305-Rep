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
  select(permno, month, mktcap) |>
  collect()

data <- crsp_monthly

# Monthly updated ---------------------------------------------------------

# mktcap is the market captilization at the end of month t (in Million)
# size is the natural log of mktcap
data <- data |>
  mutate(
    size = log(mktcap),
  )

# Annually updated (FF1992) -----------------------------------------------

# mktcap_ff is the market captilization at the end of June t (in Million)
# lasting to May t+1

mktcap_ff <- data |>
  group_by(permno, year = year(month)) |>
  mutate(
    mktcap_ff = if_else(month(month) == 6, mktcap, NA_real_),
  ) |>
  summarise(
    mktcap_ff = last(mktcap_ff, na_rm = TRUE),
  )

data <- data |>
  mutate(year = year(month)) |>
  left_join(
    mktcap_ff,
    by = c("permno", "year")
  ) |>
  select(-year)

# Shift mktcap_ff by 5 months
data <- data |>
  left_join(
    data |> select(permno, month, tmp = mktcap_ff) |> mutate(month = month + months(5)),
    by = c("permno", "month")
  ) |>
  select(-mktcap_ff) |>
  rename(mktcap_ff = tmp)

# size_ff is the natural log of mktcap_ff
data <- data |>
  mutate(
    size_ff = log(mktcap_ff),
  )


# Summary statistics ------------------------------------------------------

source("r/utils.R")
sum_stats(
  data |> rename(date = month),
  c("mktcap", "size", "mktcap_ff", "size_ff")
)

cor_stats(
  data |> rename(date = month),
  c("mktcap", "size", "mktcap_ff", "size_ff")
) |> signif(2)

# Save --------------------------------------------------------------------

dbWriteTable(
  db,
  "factor_size",
  value = data |> select(permno, month, size, size_ff),
  overwrite = TRUE
)
