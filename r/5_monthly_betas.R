source("r/lib.R")

library(knitr)
library(kableExtra)
library(stargazer)

# Load data ---------------------------------------------------------------

crsp_daily <- tbl(db, "crsp_daily") |>
  select(permno, date, month, ret_excess) |>
  collect() |>
  drop_na()

factors_ff3_daily <- tbl(db, "factors_ff3_daily") |>
  select(date, mkt_excess, smb, hml) |>
  collect()

# Join the two
crsp_daily <- crsp_daily |>
  left_join(factors_ff3_daily, by = c("date")) |>
  select(permno, date, month, ret_excess, mkt_excess, smb, hml)

# Nest into monthly time-series
panel <- crsp_daily |>
  nest(tsm = c(date, ret_excess, mkt_excess, smb, hml))

# Functions ---------------------------------------------------------------

# Estimate beta by a time series regression of excess returns on factors
# ts: time-series data frame
# return: list of coefficients (elements are NA if not enough observations)
estimate_beta <- function(ts, min_obs = 1) {
  if (nrow(ts) < min_obs) {
    return(data.frame(alpha = NA, beta_mkt = NA, beta_smb = NA, beta_hml = NA, res_std = NA))
  } else {
    fit <- lm(ret_excess ~ mkt_excess + smb + hml, data = ts)
    c <- coefficients(fit)
    return(data.frame(alpha = c[1], beta_mkt = c[2], beta_smb = c[3], beta_hml = c[4], res_std = sd(residuals(fit))))
  }
}

# Sanity Check ------------------------------------------------------------

examples <- tribble(
  ~permno, ~company,
  14593, "Apple",
  10107, "Microsoft",
  93436, "Tesla",
  17778, "Berkshire Hathaway"
)

examples_beta <- panel |>
  inner_join(examples, by = "permno") |>
  mutate(betas = map(
    tsm,
    ~ estimate_beta(.x, min_obs = 17)
  )) |>
  unnest(betas) |>
  drop_na()

examples_beta

examples_beta |>
  ggplot(aes(
    x = month,
    y = beta_mkt,
    color = company,
    linetype = company
  )) +
  geom_line() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
  )

# Run ---------------------------------------------------------------------

panel_betas <- panel %>%
  mutate(betas = pblapply(tsm, function(x) {
    x <- x %>% drop_na()
    estimate_beta(x, min_obs = 17)
  })) %>%
  unnest(betas) %>%
  drop_na()

# Shift by one month
panel_betas$month <- panel_betas$month %m+% months(1)

# Save
panel_betas <- panel_betas %>% select(-tsm)
dbWriteTable(db, "betas_ff3", panel_betas, overwrite = TRUE)

