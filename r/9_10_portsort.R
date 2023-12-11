source('r/lib.R')

# Load Data -------------------------------------------------------------------

crsp_monthly <- tbl(db, "crsp_monthly") %>%
  select(permno, month, mktcap, ret_excess, exchcd, prc) %>%
  collect()

factors <- tbl(db, "factors_size_bm_mom") %>%
  select(permno, month, size, bm, mom) %>%
  collect()

factors_ff3_monthly <- tbl(db, "factors_ff3_monthly") %>%
  select(month, mkt_excess) %>%
  collect()

data <- crsp_monthly %>%
  inner_join(factors, by = c("permno", "month"))

# Lead the return
data <- data %>%
  left_join(
    data %>% select(permno, month, ret_excess_lead = ret_excess) %>% mutate(month = month %m-% months(1)),
    by = c("permno", "month")
  ) %>%
  select(-ret_excess) %>%
  rename(ret_excess = ret_excess_lead) %>%
  drop_na()

# Functions ---------------------------------------------------------------

# Assign portfolio to the cross-sectional stocks
# data: cross-sectional
# sort_variable: variable for sorting
# n_portfolios: number of portfolios to assign
# sort_data: data used to calculate the breakpoints, default to data
#
# return: a vector of assigned portfolio corresponding to data
assign_portfolio <- function(
    data,
    sort_variable,
    n_portfolios
) {
  breakpoints <- data %>%
    pull({{ sort_variable }}) %>%
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  assigned_portfolios <- data %>%
    mutate(portfolio = findInterval(
      pick(everything()) %>% pull({{ sort_variable }}),
      breakpoints,
      all.inside = TRUE
    )) %>%
    pull(portfolio)
  
  return(assigned_portfolios)
}

# Single portfolio sort in one period
# data: cross-sectional
# ...same as above
#
# return: a dataframe of (portfolio, ew_ret, vw_ret); 
#         portfolio includes long-short (indexed by 0)
single_sort <- function(
    data,
    sort_variable,
    n_portfolios
){
  portfolios <- data %>%
    drop_na() %>%
    mutate(
      portfolio = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable }},
        n_portfolios = n_portfolios
      ) %>%
        as.factor()
    ) %>%
    group_by(portfolio) %>%
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap),
      .groups = "drop"
    )
    
  long_short <- portfolios %>%
    arrange(portfolio) %>%
    select(-portfolio) %>%
    slice(c(1, n())) %>%
    summarise(across(everything(), diff)) %>%
    mutate(portfolio = c(as.factor(0)))
  
  portfolios <- bind_rows(portfolios, long_short)
  
  return (portfolios)
}

newey_west_t_stat <- function(ts, colname) {
  model <- lm(ts[[colname]] ~ 1)  
  summary_model <- summary(model, vcov = NeweyWest(model))
  summary_model$coefficients["(Intercept)", "t value"]
}

single_sort_panel <- function(
    panel, # panel data
    sort_variable,
    n_portfolios
) {
  panel %>%
    nest(.by = month, .key = "cs") %>%
    mutate(
      portfolio_ret = map(cs, single_sort, {{ sort_variable }}, n_portfolios)
    ) %>%
    unnest(portfolio_ret) %>%
    nest(.by = portfolio, .key = "ts") %>%
    mutate(
      avg_ew_ret = map_dbl(ts, ~ mean(.x$ew_ret)),
      # var_ew_ret = map_dbl(ts, ~ var(.x$ew_ret)),
      ew_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "ew_ret")),
      avg_vw_ret = map_dbl(ts, ~ mean(.x$vw_ret)),
      # var_vw_ret = map_dbl(ts, ~ var(.x$vw_ret)),
      vw_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "vw_ret"))
    ) %>%
    select(-ts)
}

double_sort <- function(
    cs,
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    is_dependent = TRUE
){
  portfolios <- cs %>%
    drop_na() %>%
    mutate(
      portfolio_1 = assign_portfolio(
        data = pick(everything()),
        sort_variable = {{ sort_variable_1 }},
        n_portfolios = n_portfolios_1
      ) %>% as.factor()
    )
  
  # Function to assign portfolio_2
  mutate_op <- function(data1) {
    data1 %>%
      mutate(
        portfolio_2 = assign_portfolio(
          data = pick(everything()),
          sort_variable = {{ sort_variable_2 }},
          n_portfolios = n_portfolios_1
        ) %>% as.factor()
      )
  }
  
  # Conditional application
  if (is_dependent) {
    portfolios <- portfolios %>% group_by(portfolio_1) %>% mutate_op() %>% ungroup()
  } else {
    portfolios <- portfolios %>% mutate_op()
  }
  
  portfolios <- portfolios %>%
    group_by(portfolio_1, portfolio_2) %>%
    summarize(
      ew_ret = mean(ret_excess),
      vw_ret = weighted.mean(ret_excess, mktcap),
      .groups = "drop"
    )

  # Add a new long-short portfolio to data
  add_long_short <- function(data, portfolio_var) {
    portfolio_var_sym <- rlang::ensym(portfolio_var) # Convert to symbol
    
    long_short <- data %>%
      arrange({{portfolio_var}}) %>%
      select(-{{portfolio_var}}) %>%
      slice(c(1, n())) %>%
      summarise(across(everything(), diff)) %>%
      mutate(!!portfolio_var_sym := as.factor(c(0)))
    
    data <- bind_rows(data, long_short)
    
    return (data)
  }
  
  portfolios <- portfolios %>%
    nest(.by = portfolio_1) %>%
    mutate(data = map(data, add_long_short, portfolio_2)) %>%
    unnest(data) %>%
    {
      if (is_dependent) {
        .
      } else {
        nest(., .by = portfolio_2) %>%
        mutate(data = map(data, add_long_short, portfolio_1)) %>%
        unnest(data)
      }
    }

  return (portfolios)
}

double_sort_panel <- function(
    panel, # panel data
    sort_variable_1,
    sort_variable_2,
    n_portfolios_1,
    n_portfolios_2 = n_portfolios_1,
    is_dependent = TRUE
) {
  panel %>%
    nest(.by = month, .key = "cs") %>%
    mutate(
      portfolio_ret = map(cs, double_sort, {{ sort_variable_1 }}, {{ sort_variable_2 }}, n_portfolios_1, n_portfolios_2, is_dependent)
    ) %>%
    unnest(portfolio_ret) %>%
    nest(.by = c(portfolio_1, portfolio_2), .key = "ts") %>%
    mutate(
      avg_ew_ret = map_dbl(ts, ~ mean(.x$ew_ret)),
      ew_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "ew_ret")),
      avg_vw_ret = map_dbl(ts, ~ mean(.x$vw_ret)),
      vw_tstat = map_dbl(ts, ~ newey_west_t_stat(.x, "vw_ret"))
    ) %>%
    select(-ts)
}

single_sort_panel(data, size, 5)
single_sort_panel(data, bm, 5)

# test_cs <- data %>%
#   filter(month == as.Date("1997-01-01")) %>%
#   select(-month)
# 
# double_sort(test_cs, size, bm, 5, 5)
# double_sort(test_cs, size, bm, 5, 5, is_dependent = FALSE)

double_sort_panel(data, size, bm, 5, 5)
double_sort_panel(data, size, bm, 5, 5, is_dependent = FALSE)

# Data to test MOM effect
mom_data <- data %>%
  left_join(
    data %>%
      filter(exchcd %in% c(1, 31)) %>%
      group_by(month) %>%
      summarize(
        size_10th = quantile(size, probs = 0.1, na.rm = TRUE),
        .groups = "drop"
      ), # NYSE breakpoints
    by = "month"
  ) %>%
  filter(size >= size_10th) %>%
  filter(prc > 5) %>%
  select(-size_10th)

single_sort_panel(mom_data, mom, 5)
double_sort_panel(mom_data, size, mom, 5, 5)
double_sort_panel(mom_data, size, mom, 5, 5)

# Generate result table ---------------------------------------------------

# Single sort. Reference BEM Table 5.8
create_single_sort_result <- function(n_portfolio) {
  sort_size <- single_sort_panel(data, size, n_portfolio)
  sort_bm <- single_sort_panel(data, bm, n_portfolio)
  sort_mom <- single_sort_panel(mom_data, mom, n_portfolio)
  
  # merge into one dataframe
  result_single_sort <- bind_rows(
    sort_size %>% mutate(sort_variable = "Size"),
    sort_bm %>% mutate(sort_variable = "BM"),
    sort_mom %>% mutate(sort_variable = "MOM")
  ) %>% select(sort_variable, everything()) %>% 
    # tidy the data
    pivot_longer(cols = c(avg_ew_ret, ew_tstat, avg_vw_ret, vw_tstat), names_to = "statistic", values_to = "value")
  
  return(result_single_sort)
}

dbWriteTable(db, "artifact_9_single_sort_5", create_single_sort_result(5), overwrite = TRUE)
dbWriteTable(db, "artifact_9_single_sort_10", create_single_sort_result(10), overwrite = TRUE)

# For double sort, just report dependent sort on Size and BM
# Reference BEM Table 5.15
result_double_sort <- double_sort_panel(data, size, bm, 5, 5) |>
  rename("Size" = portfolio_1, "BM" = portfolio_2) |>
  pivot_longer(cols = c(avg_ew_ret, ew_tstat, avg_vw_ret, vw_tstat), names_to = "statistic", values_to = "value")

dbWriteTable(db, "artifact_10_double_sort_Size_BM_5", result_double_sort, overwrite = TRUE)
