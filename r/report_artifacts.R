library(tidyverse)
library(RSQLite)
library(stargazer)

db <- dbConnect(
  SQLite(),
  "data/main.sqlite",
  extended_types = TRUE
)

# Single Sort
result_single_sort <- tbl(db, "artifact_9_single_sort_5") %>%
  collect() %>% 
  mutate(value = case_when(
    statistic %in% c("avg_ew_ret", "avg_vw_ret") ~ paste0(round(100 * value, 2)), # NOT toString()
    statistic %in% c("ew_tstat", "vw_tstat") ~ paste0("(", round(value, 2), ")")
  )) %>%
  pivot_wider(names_from = portfolio, values_from = value) %>% 
  rename('5-1' = '0', 'Sort Variable' = sort_variable)

# EW
result_single_sort %>%
  filter(statistic %in% c("avg_ew_ret", "ew_tstat")) %>%
  select(-statistic) %>%
  mutate('Sort Variable' = c("Size", "", "BM", "", "Momentum", "")) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Single Portfolio Sort (EW)",
    label = "tab:single_sort_ew",
    rownames = FALSE
  ) %>% 
  as.character() %>%
  cat(file = "report/tabs/single_sort_ew.tex")

# VW
result_single_sort %>%
  filter(statistic %in% c("avg_vw_ret", "vw_tstat")) %>%
  select(-statistic) %>%
  mutate('Sort Variable' = c("Size", "", "BM", "", "Momentum", "")) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Single Portfolio Sort (VW)",
    label = "tab:single_sort_vw",
    rownames = FALSE
  ) %>% 
  as.character() %>%
  cat(file = "report/tabs/single_sort_vw.tex")

# Double sort
result_double_sort <- tbl(db, "artifact_10_double_sort_Size_BM_5") %>%
  collect() %>% 
  mutate(value = case_when(
    statistic %in% c("avg_ew_ret", "avg_vw_ret") ~ paste0(round(100 * value, 2)),
    statistic %in% c("ew_tstat", "vw_tstat") ~ paste0("(", round(value, 2), ")")
  )) %>%
  pivot_wider(names_from = BM, values_from = value) %>% 
  rename('5-1' = '0')

# EW
result_double_sort %>%
  filter(statistic %in% c("avg_ew_ret", "ew_tstat")) %>%
  select(-statistic) %>%
  mutate(Size = c('1', '', '2', '', '3', '', '4', '', '5', '')) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Double Portfolio Sort (EW)",
    label = "tab:double_sort_ew",
    rownames = FALSE
  ) %>% 
  as.character() %>%
  cat(file = "report/tabs/double_sort_ew.tex")

# VW
result_double_sort %>%
  filter(statistic %in% c("avg_vw_ret", "vw_tstat")) %>%
  select(-statistic) %>%
  mutate(Size = c('1', '', '2', '', '3', '', '4', '', '5', '')) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Double Portfolio Sort (VW)",
    label = "tab:double_sort_vw",
    rownames = FALSE
  ) %>% 
  as.character() %>%
  cat(file = "report/tabs/double_sort_vw.tex")


# FM
tbl(db, "artifact_11_fm") %>% 
  collect() %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(
    estimate = signif(estimate * 100, 3),
    estimate = case_when(
      abs(nw_t_stat) >= 2.57 ~ paste0(estimate, "***"),
      abs(nw_t_stat) >= 1.96 ~ paste0(estimate, "**"),
      abs(nw_t_stat) >= 1.64 ~ paste0(estimate, "*"),
      TRUE ~ as.character(estimate)
    ),
    nw_t_stat = sprintf("(%s)", round(nw_t_stat, 2))
  ) %>% 
  pivot_longer(cols = c("estimate", "nw_t_stat"), names_to = "statistic") %>% 
  pivot_wider(names_from = spec) %>%
  mutate(factor = ifelse(row_number() %% 2, factor, "")) %>% 
  select(-statistic) %>% 
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Fama-MacBeth Regression",
    label = "tab:fm",
    rownames = FALSE
  ) %>%
  as.character() %>%
  cat(file = "report/tabs/fm.tex")

# FM Arbitrage
tbl(db, "artifact_15_fm_arbitrage") %>% 
  collect() %>% 
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(
    estimate = signif(estimate * 100, 3),
    estimate = case_when(
      abs(nw_t_stat) >= 2.57 ~ paste0(estimate, "***"),
      abs(nw_t_stat) >= 1.96 ~ paste0(estimate, "**"),
      abs(nw_t_stat) >= 1.64 ~ paste0(estimate, "*"),
      TRUE ~ as.character(estimate)
    ),
    nw_t_stat = sprintf("(%s)", round(nw_t_stat, 2))
  ) %>% 
  pivot_longer(cols = c("estimate", "nw_t_stat"), names_to = "statistic") %>% 
  pivot_wider(names_from = spec) %>%
  mutate(factor = ifelse(row_number() %% 2, factor, "")) %>% 
  select(-statistic) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Fama-MacBeth Regression on Arbitrage Measures",
    label = "tab:fm_arbitrage",
    rownames = FALSE
  ) %>%
  as.character() %>%
  cat(file = "report/tabs/fm_arbitrage.tex")


# Summary Statistics ------------------------------------------------------

# Betas
tbl(db, "betas_ff3") %>%
  collect() %>%
  rename(date = month) %>% 
  sum_stats(., c("beta_mkt", "beta_smb", "beta_hml", "res_std")) %>%
  mutate(across(is.numeric, \(x) round(x, 2))) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Summary Statistics of FF3 Betas",
    label = "tab:summary_ff3_betas",
    rownames = FALSE,
    column.sep.width = "3pt", # to reduce column width
    font.size = "small" # to make font size smaller
  ) %>% 
  as.character() %>%
  cat(file = "report/tabs/summary_ff3_betas.tex")

# Size, BM, MOM
tbl(db, "factors_size_bm_mom") %>%
  collect() %>% 
  rename(date = month) %>%
  sum_stats(., c("size", "bm", "mom")) %>% 
  mutate(across(is.numeric, \(x) round(x, 1))) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Summary Statistics of Size, BM, and MOM",
    label = "tab:summary_size_bm_mom",
    rownames = FALSE,
    column.sep.width = "3pt", # to reduce column width
    font.size = "small" # to make font size smaller
  ) %>%
  as.character() %>%
  cat(file = "report/tabs/summary_size_bm_mom.tex")

# Arbitrage Measures
tbl(db, "amihud_illiq") %>% collect() %>%
  inner_join(tbl(db, "crsp_io") %>% collect(), by = c("permno", "month")) %>% 
  inner_join(tbl(db, "crsp_disp") %>% collect(), by = c("permno", "month")) %>% 
  rename(date = month) %>%
  sum_stats(., c("amihud_illiq", "inst_own", "disp")) %>% 
  mutate(across(is.numeric, \(x) round(x, 2))) %>%
  stargazer(
    type = "latex",
    summary = FALSE,
    align = TRUE,
    header = FALSE,
    title = "Summary Statistics of Arbitrage Measures",
    label = "tab:summary_arbitrage_measures",
    rownames = FALSE,
    column.sep.width = "3pt", # to reduce column width
    font.size = "small" # to make font size smaller
  ) %>%
  as.character() %>%
  cat(file = "report/tabs/summary_arbitrage_measures.tex")
  