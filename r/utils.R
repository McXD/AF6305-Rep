library(moments)

sum_stats <- function(df, cols) {
  # Initialize an empty data frame to store results
  results <- data.frame()
  
  for (col in cols) {
    result <- df |>
      group_by(date) |>
      summarise(
        mean = mean(!!rlang::sym(col)),
        sd = sd(!!rlang::sym(col)),
        skew = skewness(!!rlang::sym(col)),
        kurtosis = kurtosis(!!rlang::sym(col)),
        min = min(!!rlang::sym(col)),
        `5%` = quantile(!!rlang::sym(col), 0.05),
        `25%` = quantile(!!rlang::sym(col), 0.25),
        median = median(!!rlang::sym(col)),
        `75%` = quantile(!!rlang::sym(col), 0.75),
        `95%` = quantile(!!rlang::sym(col), 0.95),
        max = max(!!rlang::sym(col)),
        n = n()
      ) |>
      summarise(
        mean = mean(mean),
        sd = mean(sd),
        skew = mean(skew),
        kurtosis = mean(kurtosis),
        min = mean(min),
        `5%` = mean(`5%`),
        `25%` = mean(`25%`),
        median = mean(median),
        `75%` = mean(`75%`),
        `95%` = mean(`95%`),
        max = mean(max),
        n = floor(mean(n))
      )
    
    result$var <- col  # Add the column name to the result
    result <- result |> select(var, everything()) # Move the column name to the front
    results <- rbind(results, result)  # Append the result to the results data frame
    
  }
  
  return(results)
}