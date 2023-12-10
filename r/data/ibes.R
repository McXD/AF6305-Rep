# This script source and clean IBES summary data from csv file downloaded from WRDS.

source("r/data/common.R")

# Read CSV
ibes <- read_csv("data/ibes_summary.csv") |>
  select(
    tiker = TICKER,
    cusip = CUSIP,
    fpedats = FPEDATS,
    statpers = STATPERS,
    meanest = MEANEST,
    stdev = STDEV,
  ) |>
  filter(stdev != 0) |>
  arrange(cusip, fpedats, statpers)

# Save
dbWriteTable(db, "ibes", ibes, overwrite = TRUE)
