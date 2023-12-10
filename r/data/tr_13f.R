# This script source and clean data from csv file downloaded from WRDS.
# I only found tr_13f schema and s34 tables in the database. But there are
# additional institutional holding columns and I don't where they come from.
# Hence I didn't use Postgres to download data.

source("r/data/common.R")

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
