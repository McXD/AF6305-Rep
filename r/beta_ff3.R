library(tidyverse)
library(slider)
library(furrr)
library(RSQLite)
library(progressr)
library(knitr)
library(kableExtra)
library(stargazer)

# Load data ---------------------------------------------------------------

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

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

# Nest by permno
crsp_daily_nested <- crsp_daily |>
  nest(rets = c(date, month, ret_excess, mkt_excess, smb, hml))

# Functions ---------------------------------------------------------------

# Data is a time-series of returns of a single estimation period
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    return(list(alpha = NA, beta_mkt = NA, beta_smb = NA, beta_hml = NA, res_std = NA))
  } else {
    fit <- lm(ret_excess ~ mkt_excess + smb + hml, data = data)
    c <- coefficients(fit)
    return(list(alpha = c[1], beta_mkt = c[2], beta_smb = c[3], beta_hml = c[4], res_std = sd(residuals(fit))))
  }
}

# Data is a stock's time-series returns for which to calculate betas
roll_capm_estimation <- function(data, months, min_obs) {
  data <- data |>
    arrange(month)

  betas <- slide_period_dfr(
    .x = data,
    .i = data$month, # index
    .period = "month", # aggregation is applied to each month (monthly betas)
    .f = ~ estimate_capm(., min_obs),
    .before = months - 1,
    .complete = FALSE # ignore incomplete periods
  )

  betas$month <- unique(data$month)

  return(betas)
}

# Sanity Check ------------------------------------------------------------

examples <- tribble(
  ~permno, ~company,
  14593, "Apple",
  10107, "Microsoft",
  93436, "Tesla",
  17778, "Berkshire Hathaway"
)

examples_beta <- crsp_daily_nested |>
  inner_join(examples, by = "permno") |>
  mutate(betas = map(
    rets,
    ~ roll_capm_estimation(., months = 1, min_obs = 17)
  )) |>
  unnest(betas) |>
  unnest(c(alpha, beta_mkt, beta_smb, beta_hml, res_std)) |>
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

plan(multisession, workers = 8)

specs <- tibble(
  periods = c(1, 3, 6, 12, 24),
  min_obs = c(17, 51, 102, 204, 408)
)

for (i in 1:nrow(specs)) {
  with_progress({
    p <- progressor(steps = nrow(crsp_daily_nested))
    print(paste0("Running ", specs$periods[i], " month betas"))

    betas <- crsp_daily_nested |>
      mutate(betas = future_map(
        rets,
        ~ {
          p()
          roll_capm_estimation(., months = specs$periods[i], min_obs = specs$min_obs[i])
        }
      )) |>
      unnest(betas) |>
      unnest(c(alpha, beta_mkt, beta_smb, beta_hml, res_std)) |>
      drop_na() |>
      select(-rets) |>
      relocate(permno, month)
  })

  dbWriteTable(db, paste0("betas_ff3_", specs$periods[i], "m"), betas, overwrite = TRUE)
}

# Calc Summary Stats for BETA_MKT -----------------------------------------

# Join beta_mkt from different specs to a single data frame
for (i in 1:nrow(specs)) {
  if (i == 1) {
    betas_all <- tbl(db, paste0("betas_ff3_", specs$periods[i], "m")) |>
      select(permno, month, beta_mkt) |>
      collect() |>
      rename_with(~ paste0(specs$periods[i], "m"), beta_mkt)

    next
  }

  betas_all <- betas_all |>
    left_join(
      tbl(db, paste0("betas_ff3_", specs$periods[i], "m")) |> select(permno, month, beta_mkt) |> collect(),
      by = c("permno", "month")
    ) |>
    rename_with(~ paste0(specs$periods[i], "m"), beta_mkt)
}

# Summary Stats
source("r/utils.R")

beta_stats <- sum_stats(betas_all |> rename(date = month), c("1m", "3m", "6m", "12m", "24m"))

# Correlation Matrix
beta_cor_mat_p <- betas_all |>
  drop_na() |>
  select(-permno, -month) |>
  cor(method = "p")

beta_cor_mat_s <- betas_all |>
  drop_na() |>
  select(-permno, -month) |>
  cor(method = "s")

# Merge the two, place Spearman in the upper triangle and Pearson in the lower
beta_cor_mat <- beta_cor_mat_p
beta_cor_mat[upper.tri(beta_cor_mat, diag = TRUE)] <- beta_cor_mat_s[upper.tri(beta_cor_mat_s, diag = TRUE)]

# Persistence Matrix
lags <- c(1, 3, 6, 12, 24, 36, 48, 60, 120)
persistences <- tibble()

for (i in 1:length(lags)) {
  betas_lag <- betas_all |>
    mutate(month = month %m+% months(lags[i])) |>
    rename(`1m_lag` = `1m`, `3m_lag` = `3m`, `6m_lag` = `6m`, `12m_lag` = `12m`, `24m_lag` = `24m`)

  tmp <- betas_all |>
    left_join(betas_lag, by = c("permno", "month"))

  tmp <- tmp |>
    group_by(month) |>
    drop_na() |>
    summarise(
      cor_1m = cor(`1m`, `1m_lag`),
      cor_3m = cor(`3m`, `3m_lag`),
      cor_6m = cor(`6m`, `6m_lag`),
      cor_12m = cor(`12m`, `12m_lag`),
      cor_24m = cor(`24m`, `24m_lag`)
    ) |>
    summarise(across(starts_with("cor"), mean))

  tmp$lag <- lags[i]

  persistences <- bind_rows(persistences, tmp)
}

persistences <- persistences |> relocate(lag)

# Replace with NA for upper triangular due to autocorrelation
persistences[2:6][upper.tri(persistences[2:6], diag = FALSE)] <- NA

# Stargazer ---------------------------------------------------------------

beta_stats %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(var = substr(var, 1, nchar(var) - 1)) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Beta Summary Statistics",
    rownames = FALSE,
    covariate.labels = c("Months", "$\\mu$", "$\\sigma$"),
    label = "tab:beta_summary_stats"
  ) %>%
  as.character() %>%
  cat(file = "tex/beta_summary_stats.tex", sep = "\n")

diag(beta_cor_mat) <- NA
beta_cor_mat %>%
  round(2) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Beta Correlation Matrix",
    label = "tab:beta_cor_mat"
  ) %>%
  as.character() %>%
  cat(file = "tex/beta_cor_mat.tex", sep = "\n")

persistences %>%
  mutate_if(is.numeric, round, 2) %>%
  # convert to string, NA is empty
  mutate(across(starts_with("cor"), as.character)) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Beta Persistence",
    label = "tab:beta_persistence",
    rownames = FALSE,
    covariate.labels = c("Lag", "$\\beta^{1M}$", "$\\beta^{3M}$", "$\\beta^{6M}$", "$\\beta^{12M}$", "$\\beta^{24M}$")
  ) %>%
  as.character() %>%
  cat(file = "tex/beta_persistence.tex", sep = "\n")
