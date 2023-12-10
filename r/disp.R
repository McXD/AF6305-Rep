# This script calculates analyst dispersion

# Read data
db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

ibes <- tbl(db, "ibes") |> collect()

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
