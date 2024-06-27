

data_information <- function(data) {
  if (!is.data.frame(data)) {
    cat("The input is not a data frame.\n")
    return(NULL)
  }
  
  cat("Basic Information about the Dataset:\n")
  cat("-----------------------------\n")
  
  # Dimensions of the dataset
  cat("Dimensions (rows x columns): ",
      dim(data)[1],
      " x ",
      dim(data)[2],
      "\n\n")
  
  # Column names
  cat("Column Names:\n")
  cat(paste(names(data), collapse = ", "), "\n\n")
  
  # Summary of each column
  cat("Summary of each column:\n")
  for (col in names(data)) {
    cat(col, ":\n")
    cat(paste(capture.output(summary(data[[col]])), collapse = "\n"), "\n\n")
  }
  
  # Number of missing values
  cat("Number of missing values in each column:\n")
  missing_values <- colSums(is.na(data))
  for (col in names(missing_values)) {
    cat(col, ": ", missing_values[col], "\n")
  }
  
  # Data types of each column
  cat("\nData types of each column:\n")
  for (col in names(data)) {
    cat(col, ": ", class(data[[col]]), "\n")
  }
}
data_information(iris)




library(vcd)

# Fonction pour calculer le V de Cramer
cramers_v <- function(x, y) {
  chi2 <- chisq.test(table(x, y))
  n <- sum(table(x, y))
  phi2 <- chi2$statistic / n
  r <- nrow(table(x, y))
  k <- ncol(table(x, y))
  v <- sqrt(phi2 / min(r - 1, k - 1))
  return(v)
}
categorical_corelation(train )

# Fonction pour créer la matrice des V de Cramer
categorical_corelation <- function(data, threshold = 0.7) {
  cat_data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    dplyr::select(where(is.factor))
  
  cat_vars <- cat_data  %>% colnames()
  
  cramers_v_matrix <- matrix(
    0,
    nrow = length(cat_vars),
    ncol = length(cat_vars),
    dimnames = list(cat_vars, cat_vars)
  )
  
  for (i in 1:length(cat_vars)) {
    for (j in 1:length(cat_vars)) {
      if (i != j) {
        cramers_v_matrix[i, j] <- cramers_v(cat_data[[i]], cat_data[[j]])
      }
    }
  }
  
  adj_matrix <- abs(cramers_v_matrix) > threshold
  diag(adj_matrix) <- 0
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  graph1 <- plot(cor_graph)
  
  components <- components(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create a data frame to list the variables in each connected component
  connected_vars <- data.frame(variable = names(membership), component = membership)
  
  # Group and print the connected variables
  connected_groups <- connected_vars %>%
    group_by(component) %>%
    summarise(variables = paste(variable, collapse = ", ")) %>%
    filter(sapply(strsplit(variables, "\\s+"), length) > 1)
  
  
  
  return(list(data = connected_groups, graph1 = graph1))
  
}


create_chi2_pvalue_matrix <- function(data) {
  cat_data <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    dplyr::select(where(is.factor))
  
  cat_vars <- cat_data %>% colnames()
  pvalue_matrix <- matrix(
    0,
    nrow = length(cat_vars),
    ncol = length(cat_vars),
    dimnames = list(cat_vars, cat_vars)
  )
  
  for (i in 1:length(cat_vars)) {
    for (j in 1:length(cat_vars)) {
      if (i != j) {
        chi2_test <- chisq.test(table(cat_data[[i]], cat_data[[j]]))
        
        # Interprétation
        
        if (chi2_test$p.value < 0.05) {
          cat("Nous rejetons H0, liaisons entre : ",
              cat_vars[i],
              "et ",
              cat_vars[j],
              " .\n")
        }
        pvalue_matrix[i, j] <- chi2_test$p.value
      }
    }
  }
  
  return(pvalue_matrix)
}




library(broom)

viz_lm_model_1 <- function(model, n) {
  model_summary <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    arrange(p.value) %>%
    head(n)
  ggplot(model_summary, aes(x = term, y = estimate)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "Importance des variables dans le modèle de régression", x = "Variables", y = "Estimation des coefficients") +
    theme_minimal()
}

viz_rf_model_2 <- function(model) {
  p1 <- importance(model) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    ggplot(aes(x = term, y = X.IncMSE)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "random forest fearture importance", x = "", y = "%IncMSE") +
    theme_minimal()
  
  p2 <- importance(model) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    ggplot(aes(x = term, y = IncNodePurity)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "random forest fearture importance", x = "", y = "IncNodePurity") +
    theme_minimal()
  
  p1 + p2
}

plot.res = function(modele_1, titre = "") {
  plot(
    predict(modele_1),
    residuals(modele_1),
    col = "black",
    ylab = "Résidus",
    xlab = "Valeurs predites",
    main = titre
  )
  abline(h = 0, col = "red", lwd = 2)
}

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








is_discretisable <- function(data, var, label) {
  var_name <- as_label(enquo(var))
  label_name <- as_label(enquo(label))
  
  
  formula_regress <- as.formula(paste(label_name, "~", var_name))
  formula_cart <- formula_regress
  formula_poly <- as.formula(paste(label_name, " ~ poly(", var_name, ", 2, raw = TRUE)"))
  
  
  
  model_regress <- lm(formula_regress, data = data)
  model_regress_summary <- summary(model_regress)
  r_regress_squared <- model_regress_summary$r.squared
  
  
  
  model_poly <- lm(formula_poly, data = data)
  model_poly_summary <- summary(model_poly)
  r_squared_poly <- model_poly_summary$r.squared
  
  
  cart_model <- rpart(
    formula_cart,
    data = data,
    method = "anova",
    control = rpart.control(minsplit = 20, minbucket = 15)
  )
  # minsplit: Minimum number of observations that must exist in a node in order for a split to be attempted.
  # minbucket: Minimum number of observations in any terminal (leaf) node. This controls the complexity of the tree.
  
  # Assigner chaque observation à un nœud
  data$node <- cart_model$where
  
  # Calculer la moyenne de chaque nœud
  node_means <- aggregate(as.formula(paste(label_name, "~ node")), data = data, FUN = mean)
  names(node_means)[2] <- "NodeMean"
  
  
  # Fusionner les moyennes des nœuds avec les données d'origine
  data <- merge(data, node_means, by = "node")
  
  # Créer des bar plots pour chaque nœud
  plot1 <- ggplot(data, aes(x = as.factor(node), y = {{label}})) +
    geom_bar(fill = "#219C90",
             stat = "summary",
             fun = "mean") +
    stat_summary(
      fun = "mean",
      geom = "text",
      aes(label = round(..y.., 2)),
      vjust = -0.5,
      color = "black"
    ) +
    labs(title = "",
         x = "Nœud",
         y = paste0("Moyenne de ", label_name)) +
    custom_theme
  
  # Calculer les proportions pour chaque niveau de node
  proportions <- prop.table(table(data$node)) * 100
  
  plot2 <- ggplot(data, aes(x = as.factor(node), y = {{label}})) +
    geom_boxplot(fill = "#219C90") +
    labs(title = "", x = "Nœud", y = label_name) + annotate(
      "text",
      x = as.factor(unique(data$node)),
      y = rep(max(data[[label_name]]) + 1, length(unique(data$node))),
      label = paste0(round(proportions, 1), "%"),
      vjust = -0.5,
      size = 4
    )  + custom_theme
  
  # Afficher le R^2
  sst <- sum((data[[label_name]] - mean(data[[label_name]])) ^ 2)
  sse <- sum((data[[label_name]] - data$NodeMean) ^ 2)
  r_squared <- 1 - (sse / sst)
  
  mat <- matrix(
    c(
      "regress",
      "poly     ",
      "CART  ",
      round(r_regress_squared, 3),
      round(r_squared_poly, 3),
      round(r_squared, 3)
    ),
    nrow = 3,
    byrow = FALSE
  )
  
  # Convertir la matrice en une chaîne de caractères formatée pour afficher dans un sous-titre
  subtitle_text <- paste("- ", apply(mat, 1, paste, collapse = "       "), collapse = "\n")
  
  mat2 <- matrix(
    c(
      paste0(strrep("*", 50)),
      paste0(strrep("*", 40)),
      paste0(strrep("*", 30)),
      paste0(strrep("*", 20)),
      paste0(strrep("*", 10)),
      paste0(strrep("*", 5))
    ),
    nrow = 3,
    byrow = FALSE
  )
  
  cat(paste(mat2, collapse = "\n"))
  
  cat("            Si CART améliore le R2 \n")
  
  cat(paste0(strrep("*", 5)))
  cat("            discretise la variable \n")
  
  
  
  mat2 <- matrix(
    c(
      paste0(strrep("*", 5)),
      paste0(strrep("*", 10)),
      paste0(strrep("*", 20)),
      paste0(strrep("*", 30)),
      paste0(strrep("*", 40)),
      paste0(strrep("*", 50))
    ),
    nrow = 3,
    byrow = FALSE
  )
  
  cat(paste(mat2, collapse = "\n"))
  cat("\n")
  cat("\n")
  cat(subtitle_text)
  cat("\n")
  combined_plot <- plot1 + plot2 +
    plot_layout(ncol = 2) +
    plot_annotation(title = var_name, subtitle = "")
  
  a <- sort(as.data.frame(cart_model$splits)$index)
  return(list(
    cuts = a,
    barplot = plot1,
    boxplot = plot2
  ))
  
}
is_discretisable(train, LotArea, SalePrice)

calculate_variable_importance <- function(data, percentage, n_iterations, target_variable) {
  n_variables <- ncol(data) - 1  # pour ne pas prendre la variable dépendate
  sample_size <- round(percentage * n_variables)
  
  variable_scores <- rep(0, n_variables)
  
  for (i in 1:n_iterations) {
    selected_vars <- sample(1:n_variables, sample_size)
    
    # Créer le sous-ensemble de données
    subset_data <- data[, c(selected_vars, ncol(data))]
    
    # Séparer les données en jeu d'entraînement et de test
    train_idx <- sample(1:nrow(subset_data), round(0.8 * nrow(subset_data)))
    train <- subset_data[train_idx, ]
    
    # Modèle complet
    full_model <- glm(as.formula(paste(target_variable, "~ .")), data = train, family = gaussian(link = "log"))
    
    # Régression stepwise
    step_model <- step(full_model, direction = "both", trace = 0)
    
    # Mettre à jour les scores
    selected_in_step <- names(coef(step_model))[-1]  # Exclure l'intercept
    variable_scores[selected_vars[selected_vars %in% which(names(data) %in% selected_in_step)]] <-
      variable_scores[selected_vars[selected_vars %in% which(names(data) %in% selected_in_step)]] + 1
  }
  
  importance_scores <- variable_scores / n_iterations
  
  # Trier les variables par importance décroissante
  sorted_importance <- sort(importance_scores, decreasing = TRUE)
  
  # Afficher les résultats
  print(sorted_importance)
}


calculate_variable_importance(train %>% select(LotArea, MSZoning, time, Neighborhood, MSSubClass, SalePrice), 0.6, 100,  "SalePrice")

