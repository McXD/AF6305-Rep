# This script
# 1. Source the data from ibes.csv, downloaded from WRDS (Table: 'ibes')
# 2. Calculate analyst dispersion (Table: 'crsp_disp')

source("r/lib.R")

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


# Calculate DISP ----------------------------------------------------------

disp <- ibes |>
  mutate(disp = stdev / meanest) 

# Join with crsp_monthly

disp <- disp |> mutate(month = floor_date(statpers, "month"))

crsp_monthly <- tbl(db, "crsp_monthly") |> collect()

crsp_disp <- disp |>
  left_join(crsp_monthly, by = c("cusip", "month")) |>
  select(permno, month, disp) |>
  drop_na()

# Save
dbWriteTable(db, "crsp_disp", crsp_disp, overwrite = TRUE)
