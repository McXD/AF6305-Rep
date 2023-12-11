# This script
# 1. Calculate Amihud Illiquidity (Table: 'amihud_illiq')

source("r/lib.R")

# Load data ---------------------------------------------------------------

crsp_daily <- tbl(db, "crsp_daily") |>
  select(permno, date, month, ret, vol, prc) |>
  collect() |>
  drop_na()

crsp_monthly <- tbl(db, "crsp_monthly") |>
  select(permno, month, exchcd) |>
  collect()

# Get exchange code from monthly data
crsp_daily <- crsp_daily |>
  left_join(crsp_monthly, by = c("permno", "month")) |>
  drop_na()

# Adjust volumes for Nasdaq stocks
# Prior to 2001-02-01, divide by 2
# 2001-02-01 to 2001-12-31, divide by 1.8
# 2002-01-01 to 2004-12-31, divide by 1.6
# After 2005-01-01, divide by 1 (no adjustment)
t <- crsp_daily |>
  mutate(
    vol_adj = case_when(
      exchcd %in% c(3, 33) & date < as.Date("2001-02-01") ~ 2,
      exchcd %in% c(3, 33) & date >= as.Date("2001-02-01") & date < as.Date("2001-12-31") ~ 1.8,
      exchcd %in% c(3, 33) & date >= as.Date("2002-01-01") & date < as.Date("2004-12-31") ~ 1.6,
      TRUE ~ 1
    ),
    vol = vol / vol_adj
  )

# amihub_illiq = ret/vol
# take monthly average
amihud_illiq <- t |>
  filter(vol != 0) |>
  mutate(amihud_illiq = abs(ret) / vol * prc) |>
  group_by(permno, month) |>
  summarize(
    amihud_illiq = mean(amihud_illiq, na.rm = TRUE) * 1000000,
    .groups = "drop"
  ) |>
  select(permno, month, amihud_illiq)

# Save
dbWriteTable(db, "amihud_illiq", amihud_illiq, overwrite = TRUE)

