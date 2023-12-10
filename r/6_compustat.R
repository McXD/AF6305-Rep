# This script
# 1. download compustat (Table: 'compustat')
# 2. add a compustat link column to crsp_monthly (Table: 'crsp_monthly')

source("r/lib.R")

wrds <- wrds_connect()
funda_db <- tbl(wrds, in_schema("comp", "funda"))

compustat <- funda_db |>
  filter(
    indfmt == "INDL" &
      datafmt == "STD" &
      consol == "C" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(
    gvkey, # Firm identifier
    datadate, # Date of the accounting data
    seq, # Stockholders' equity
    ceq, # Total common/ordinary equity
    at, # Total assets
    lt, # Total liabilities
    txditc, # Deferred taxes and investment tax credit
    txdb, # Deferred taxes
    itcb, # Investment tax credit
    pstkrv, # Preferred stock redemption value
    pstkl, # Preferred stock liquidating value
    pstk, # Preferred stock par value
    capx, # Capital investment
    oancf, # Operating cash flow
    sale, # Revenue
    cogs, # Costs of goods sold
    xint, # Interest expense
    xsga # Selling, general, and administrative expenses
  ) |>
  collect()

compustat <- compustat |>
  mutate(
    be = coalesce(seq, ceq + pstk, at - lt) +
      coalesce(txditc, txdb + itcb, 0) -
      coalesce(pstkrv, pstkl, pstk, 0),
    be = if_else(be <= 0, as.numeric(NA), be),
    op = (sale - coalesce(cogs, 0) -
      coalesce(xsga, 0) - coalesce(xint, 0)) / be,
  )

compustat <- compustat |>
  mutate(year = year(datadate)) |>
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |>
  ungroup()

compustat <- compustat |>
  left_join(
    compustat |>
      select(gvkey, year, at_lag = at) |>
      mutate(year = year + 1),
    by = c("gvkey", "year")
  ) |>
  mutate(
    inv = at / at_lag - 1,
    inv = if_else(at_lag <= 0, as.numeric(NA), inv)
  )

dbWriteTable(
  db,
  "compustat",
  value = compustat,
  overwrite = TRUE
)

# Add COMPUSTAT-link to CRSP ---------------------------------------------------------

ccmxpf_linktable_db <- tbl(
  wrds,
  in_schema("crsp", "ccmxpf_linktable")
)

ccmxpf_linktable <- ccmxpf_linktable_db |>
  filter(linktype %in% c("LU", "LC") &
    linkprim %in% c("P", "C") &
    usedflag == 1) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

crsp_monthly <- tbl(db, "crsp_monthly") |>
  collect()

ccm_links <- crsp_monthly |>
  inner_join(ccmxpf_linktable,
    by = "permno", relationship = "many-to-many"
  ) |>
  filter(!is.na(gvkey) &
    (date >= linkdt & date <= linkenddt)) |>
  select(permno, gvkey, date)

crsp_monthly <- crsp_monthly |>
  left_join(ccm_links, by = c("permno", "date"))

dbWriteTable(
  db,
  "crsp_monthly",
  value = crsp_monthly,
  overwrite = TRUE
)

dbDisconnect(wrds)

# EDA ---------------------------------------------------------------------

compustat <- tbl(db, "compustat") |>
  collect()

crsp_monthly <- tbl(db, "crsp_monthly") |>
  collect()

crsp_monthly |>
  mutate(exchange = case_when(
    exchcd %in% c(1, 31) ~ "NYSE",
    exchcd %in% c(2, 32) ~ "AMEX",
    exchcd %in% c(3, 33) ~ "NASDAQ",
    .default = "Other"
  )) |>
  group_by(permno, year = year(month)) |>
  filter(date == max(date)) |>
  ungroup() |>
  left_join(compustat, by = c("gvkey", "year")) |>
  group_by(exchange, year) |>
  summarize(
    share = n_distinct(permno[!is.na(be)]) / n_distinct(permno),
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = year,
    y = share,
    color = exchange,
    linetype = exchange
  )) +
  geom_line() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    title = "Share of securities with book equity values by exchange"
  ) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1))
