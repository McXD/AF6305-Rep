library(tidyverse)
library(RSQLite)
library(stargazer)

# Ggplot theme ------------------------------------------------------------

theme_set(theme_bw())
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)

# Load data ---------------------------------------------------------------

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(db, "crsp_monthly") |>
  collect() |>
  drop_na()

# Convert to human-readable form ------------------------------------------

crsp_monthly <- crsp_monthly |>
  mutate(exchange = case_when(
    exchcd %in% c(1, 31) ~ "NYSE",
    exchcd %in% c(2, 32) ~ "AMEX",
    exchcd %in% c(3, 33) ~ "NASDAQ",
    .default = "Other"
  ))

crsp_monthly <- crsp_monthly |>
  mutate(industry = case_when(
    siccd >= 1 & siccd <= 999 ~ "Agriculture",
    siccd >= 1000 & siccd <= 1499 ~ "Mining",
    siccd >= 1500 & siccd <= 1799 ~ "Construction",
    siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
    siccd >= 4000 & siccd <= 4899 ~ "Transportation",
    siccd >= 4900 & siccd <= 4999 ~ "Utilities",
    siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
    siccd >= 5200 & siccd <= 5999 ~ "Retail",
    siccd >= 6000 & siccd <= 6799 ~ "Finance",
    siccd >= 7000 & siccd <= 8999 ~ "Services",
    siccd >= 9000 & siccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))


# Number of stocks in exchanges -------------------------------------------

crsp_monthly %>%
  count(exchange, date) %>%
  bind_rows(
    crsp_monthly %>%
      group_by(date) %>%
      summarise(n = n(), exchange = "Total")
  ) %>%
  mutate(exchange = factor(exchange, levels = c("Total", "NYSE", "AMEX", "NASDAQ", "Other"))) %>%
  ggplot(aes(x = date, y = n, linetype = exchange)) +
  geom_line() +
  labs(
    x = "Year", y = "Number of Stocks", color = NULL, linetype = NULL,
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = c(0.85, 0.78))

ggsave(
  "report/figs/stock_exchange_composition.png",
  width = 8, height = 4, units = "in", dpi = 300
)

# Value of Stock in Exchanges --------------------------------------------

# TODO: adjust for inflation
crsp_monthly %>%
  group_by(exchange, date) %>%
  summarise(mktcap = sum(mktcap) / 1000) %>%
  bind_rows(
    crsp_monthly %>%
      group_by(date) %>%
      summarise(mktcap = sum(mktcap) / 1000, exchange = "Total")
  ) %>%
  mutate(exchange = factor(exchange, levels = c("Total", "NYSE", "AMEX", "NASDAQ", "Other"))) %>%
  ggplot(aes(x = date, y = mktcap, linetype = exchange)) +
  geom_line() +
  labs(
    x = "Year", y = "Total Market Capitalization (in $Billion)", color = NULL, linetype = NULL,
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma, n.breaks = 6) +
  theme(legend.position = c(0.1, 0.78))

ggsave(
  "report/figs/value_of_stock_in_exchanges.png",
  width = 8, height = 4, units = "in", dpi = 300
)

# Summary of Stock Returns ------------------------------------------------
source("r/utils.R")

crsp_daily <- tbl(db, "crsp_daily") |>
  collect() |>
  drop_na()

ss_m <- crsp_monthly %>%
  sum_stats("ret_excess") %>%
  mutate(var = "r_m")

ss_d <- crsp_daily %>%
  sum_stats("ret_excess") %>%
  mutate(var = "r_d")

ss <- bind_rows(ss_m, ss_d)

# stargazer
ss %>%
  mutate(across(mean:max, ~ signif(.x, 2))) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Summary Statistics of Returns",
    label = "tab:return_summary",
    rownames = FALSE
  ) %>%
  as.character() %>%
  cat(file = "report/tabs/return_summary.tex")
