library(moments)

sum_stats <- function(df, cols) {
  results <- data.frame()

  for (col in cols) {
    # remove NA for this column
    df_na <- df |> filter(!is.na(!!rlang::sym(col)))

    result <- df_na |>
      group_by(date) |> # for a cross-section
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
        n = sum(!is.na(!!rlang::sym(col)))
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

    result$var <- col # Add the column name to the result
    result <- result |> select(var, everything()) # Move the column name to the front
    results <- rbind(results, result) # Append the result to the results data frame
  }

  return(results)
}

cor_stats <- function(data, cols) {
  # Split the data by month
  data_by_month <- data %>% mutate(month = month(date)) %>% 
    split(.$month)
  
  # Initialize empty list to store correlation matrices
  cor_matrices <- list()
  
  # Loop over each month
  for(i in 1:length(data_by_month)) {
    # Select relevant columns and drop NA values
    data_month <- data_by_month[[i]] %>% select(all_of(cols)) %>% na.omit()
    
    # Calculate Spearman and Pearson correlations
    spearman <- cor(data_month, method = "spearman")
    pearson <- cor(data_month, method = "pearson")
    
    # Combine into one matrix, with Spearman above diagonal and Pearson below
    cor_matrix <- spearman
    cor_matrix[lower.tri(cor_matrix)] <- pearson[lower.tri(pearson)]
    
    # Store in list
    cor_matrices[[i]] <- cor_matrix
  }
  
  # Calculate time-series average of the matrices
  avg_cor_matrix <- Reduce("+", cor_matrices) / length(cor_matrices)
  
  return(avg_cor_matrix)
}