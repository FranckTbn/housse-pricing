R2_coef <- function(y_pred, y) {
  sst <- sum((y - mean(y)) ^ 2)
  sse <- sum((y_pred - y) ^ 2)
  rsq <- 1 - sse / sst
  rsq
}


cat_feature_importance <- function(data, label) {
  var_name <- as_label(enquo(label))
  
  cat_data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    dplyr::select(where(is.factor))
  
  cat_vars <- colnames(cat_data)
  #cat_vars <- cat_vars[1:3]  # Adjust the number of variables to be considered
  n <- nrow(data)
  
  R2 <- c()
  for (var in cat_vars) {
    grouped_data <- data %>%
      group_by(!!sym(var)) %>%
      summarise(mean_SalePrice = mean(!!sym(var_name), na.rm = TRUE))  # Calculate mean SalePrice for each group
    
    y <- rep(0, times = n)
    
    # assigner la moyenne du groupe à chaque modalité corespondante
    for (i in 1:n) {
      ind <- which(grouped_data[[1]] == data[[var]][i])
      y[i] <- grouped_data$mean_SalePrice[ind]
    }
    R2 <- c(R2, R2_coef(y, data[[var_name]]))
    
  }
  
  df <- data.frame(variables = cat_vars, R2 = R2)
  
  df %>%
    arrange(desc(R2)) %>%  # Arrange by R² in descending order
    mutate(variables = factor(variables, levels = rev(variables))) %>%  # Set the order of factors
    ggplot(aes(x = variables, y = R2)) +
    geom_segment(aes(xend = variables, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "Variables catégorielles qui expliquent le mieux la variance de SalePrice", x = "", y = "R²") +
    theme_minimal()
}

cat_feature_importance(train, SalePrice)