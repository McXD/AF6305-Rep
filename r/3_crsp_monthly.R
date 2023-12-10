# This script:
# 1. Download CRSP monthly data
# 2. Clean and derive fields
# 3. Table: 'crsp_monthly'

source("r/lib.R")

# Download data -----------------------------------------------------------

wrds <- wrds_connect()

msf_db <- tbl(wrds, in_schema("crsp", "msf"))
msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))
msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))

# Takes about 2 minutes
crsp_monthly <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  inner_join(
    msenames_db |>
      filter(shrcd %in% c(10, 11)) |> # US Stocks
      select(permno, exchcd, siccd, namedt, nameendt),
    by = c("permno")
  ) |>
  filter(date >= namedt & date <= nameendt) |>
  mutate(month = floor_date(date, "month")) |>
  left_join(
    msedelist_db |>
      select(permno, dlstdt, dlret, dlstcd) |>
      mutate(month = floor_date(dlstdt, "month")),
    by = c("permno", "month")
  ) |>
  select(
    permno, # Security identifier
    cusip, # To link with TR
    date, # Date of the observation
    month, # Month of the observation
    ret, # Return
    shrout, # Shares outstanding (in thousands)
    prc, # Price or negative bid/ask average on last trading day on the month
    exchcd, # Exchange code
    siccd, # Industry code
    dlret, # Delisting return
    dlstcd # Delisting code
  ) |>
  collect() |>
  mutate(
    month = ymd(month),
    shrout = shrout * 1000
  )

dbDisconnect(wrds)

# Calc Market Cap ---------------------------------------------------------

crsp_monthly <- crsp_monthly |>
  mutate(
    mktcap = abs(shrout * abs(prc)) / 10^6,
    mktcap = na_if(mktcap, 0)
  )

# Calc Adjusted Return ----------------------------------------------------

crsp_monthly <- crsp_monthly |>
  mutate(
    ret_adj = case_when(
      is.na(dlstcd) ~ ret, # ret can be NA
      !is.na(dlret) ~ dlret,
      dlstcd %in% c(500, 520, 580, 584) |
        (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
      dlstcd == 100 ~ ret,
      TRUE ~ -1
    )
  ) |>
  select(-c(dlret, dlstcd))

# Calc excess return ------------------------------------------------------

factors_ff3_monthly <- tbl(db, "factors_ff3_monthly") |>
  select(month, rf) |>
  collect()

crsp_monthly <- crsp_monthly |>
  left_join(
    factors_ff3_monthly,
    by = "month"
  ) |>
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1)
  ) |>
  select(-ret_adj, -rf)
# na.omit() # TODO: better treatment?

# Write to SQLite ---------------------------------------------------------

dbWriteTable(db, "crsp_monthly", crsp_monthly, overwrite = TRUE)

# Summary Stats -----------------------------------------------------------

source("r/utils.R")

crsp_monthly |>
  na.omit() |>
  sum_stats(c("ret", "ret_excess"))

# Trend of N
crsp_monthly |>
  group_by(month) |>
  summarize(n = n()) |>
  ggplot(aes(x = month, y = n)) +
  geom_line() +
  labs(
    title = "Number of observations",
    x = "Month",
    y = "Number of observations"
  )

# Total Market Cap
crsp_monthly |>
  group_by(month) |>
  summarize(mktcap = sum(mktcap, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = mktcap)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Total Market Cap (in million USD)"
  )

# NA Analysis -------------------------------------------------------------

stock_with_na <- crsp_monthly |>
  group_by(permno) |>
  summarize(
    n = n(),
    n_na = sum(is.na(ret_excess)),
    n_na_pct = n_na / n,
    last_listed_month = max(month),
    na_last_month = any(is.na(ret_excess) & month == last_listed_month)
  ) |>
  filter(n_na_pct > 0) |>
  arrange(desc(n_na_pct))
