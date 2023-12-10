# This script:
# 1. Source the data from tr_13f.csv, downloaded from WRDS
# 2. Construct IO measure (Table: 'crsp_io')

# This script source and clean data from csv file downloaded from WRDS.
# I only found tr_13f schema and s34 tables in the database. But there are
# additional institutional holding columns and I don't where they come from.
# Hence I didn't use Postgres to download data.

source("r/lib.R")

# Read CSV
tr_13f <- read_csv("data/tr_13f.csv") |>
  mutate(month = floor_date(rdate, "month")) |>
  select(
    cusip,
    month,
    inst_own = InstOwn_Perc
  )

# Save
dbWriteTable(db, "tr_13f", tr_13f, overwrite = TRUE)

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

t <- t|>
  drop_na() |>
  select(permno, month, inst_own)

# Save
dbWriteTable(db, "crsp_io", t, overwrite = TRUE)
