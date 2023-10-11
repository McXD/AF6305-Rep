source("r/data/common.R")

# SQLite Connection -------------------------------------------------------

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

# Download ----------------------------------------------------------------

wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USERNAME"),
  password = Sys.getenv("WRDS_PASSWORD")
)

dsf_db <- tbl(wrds, in_schema("crsp", "dsf"))

factors_ff3_daily <- tbl(db, "factors_ff3_daily") |>
  collect()

permnos <- tbl(db, "crsp_monthly") |>
  distinct(permno) |>
  pull()

chunk_size <- 100
num_chunks <- ceiling(length(permnos) / chunk_size)

pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = num_chunks
)

for (j in 1:num_chunks) {
  # Select the permnos for this chunk
  permno_chunk <- permnos[((j - 1) * 10 + 1):min(j * 10, length(permnos))]

  # Process all permnos in the chunk at once
  crsp_daily_sub <- dsf_db |>
    filter(permno %in% permno_chunk &
      date >= start_date & date <= end_date) |>
    select(permno, date, ret) |>
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
      select(permno, date, month, ret, ret_excess)

    dbWriteTable(db,
      "crsp_daily",
      value = crsp_daily_sub,
      overwrite = ifelse(j == 1, TRUE, FALSE),
      append = ifelse(j != 1, TRUE, FALSE)
    )
  }

  pb$tick()
}

# Summary Stats -----------------------------------------------------------

source('r/utils.R')

crsp_daily |>
  sum_stats(c("ret", "ret_excess"))

# Trend of N along time-series
crsp_daily |>
  group_by(date) |>
  summarise(n = n()) |>
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  labs(x = "Month", y = "Number of observations")
