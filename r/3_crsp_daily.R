# This script
# 1. Download CRSP daily data
# 2. Table: 'crsp_daily'

source("r/lib.R")

# Download ----------------------------------------------------------------

wrds <- wrds_connect()

dsf_db <- tbl(wrds, in_schema("crsp", "dsf"))

factors_ff3_daily <- tbl(db, "factors_ff3_daily") |>
  collect()

permnos <- tbl(db, "crsp_monthly") |>
  distinct(permno) |>
  pull()

# Determine the number of chunks
chunk_size <- 600
num_chunks <- ceiling(length(permnos) / chunk_size)

# Progress bar using progress package
pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = num_chunks
)

for (j in 1:num_chunks) {
  # Select the permnos for this chunk
  permno_chunk <- permnos[((j - 1) * chunk_size + 1):min(j * chunk_size, length(permnos))]

  # Process all permnos in the chunk at once
  crsp_daily_sub <- dsf_db |>
    filter(permno %in% permno_chunk &
      date >= start_date & date <= end_date) |>
    select(
      permno,
      cusip, # to link with other databases, such as TR
      date,
      ret,
      vol, # volume, to construct Amihud illiquidity
      prc
    ) |>
    collect() |>
    drop_na()

  if (nrow(crsp_daily_sub) > 0) {
    crsp_daily_sub <- crsp_daily_sub |>
      mutate(month = floor_date(date, "month")) |>
      left_join(factors_ff3_daily |>
        select(date, rf), by = "date") |>
      mutate(
        ret_excess = ret - rf,
        ret_excess = pmax(ret_excess, -1)
      ) |>
      select( # Final columns
        permno,
        cusip,
        date,
        month, 
        ret, 
        ret_excess, 
        vol,
        prc
      )

    dbWriteTable(db,
      "crsp_daily",
      value = crsp_daily_sub,
      overwrite = ifelse(j == 1, TRUE, FALSE),
      append = ifelse(j != 1, TRUE, FALSE)
    )
  }

  pb$tick()
}

dbDisconnect(wrds)

# Summary Stats -----------------------------------------------------------

crsp_daily <- tbl(db, "crsp_daily") |>
  select(permno, date, month, ret_excess) |>
  collect() |>
  drop_na()

crsp_daily |>
  sum_stats(c("ret_excess"))

# Trend of N along time-series
crsp_daily |>
  group_by(date) |>
  summarise(n = n()) |>
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  labs(x = "Month", y = "Number of observations")
