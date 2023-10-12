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

# Create a sequence of dates from start_date to end_date by month
date_seq <- seq(from = start_date, to = end_date, by = "month")

pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = length(date_seq) - 1
)

for (i in 1:(length(date_seq) - 1)) {
  # Define the start and end date for each chunk
  chunk_start_date <- date_seq[i]
  chunk_end_date <- date_seq[i + 1]
  
  # Process all permnos within the date range
  crsp_daily_sub <- dsf_db |>
    filter(permno %in% permnos &
             date >= chunk_start_date & date < chunk_end_date) |>
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
                 overwrite = ifelse(i == 1, TRUE, FALSE),
                 append = ifelse(i != 1, TRUE, FALSE)
    )
  }
  
  pb$tick()
}

dbDisconnect(wrds)

# Summary Stats -----------------------------------------------------------

source('r/utils.R')

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
