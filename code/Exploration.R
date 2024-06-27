
# gt_trabi
gt_trabi <- function(data, n = NULL) {
  if (is.null(n)) {
    data %>%
      gt() %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_column_labels(everything())) %>%
      opt_table_lines(extent = "default") %>%
      tab_options(
        column_labels.border.top.color = "white",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "black",
        table_body.hlines.color = "white",
        table.border.bottom.color = "black",
        table.border.bottom.width = px(3)
      ) %>%
      tab_source_note(md("<br>@bineneothniel.tra <br> "))
  } else{
    data %>%
      slice_head(n = n) %>%
      gt() %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_column_labels(everything())) %>%
      opt_table_lines(extent = "default") %>%
      tab_options(
        column_labels.border.top.color = "white",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "black",
        table_body.hlines.color = "white",
        table.border.bottom.color = "black",
        table.border.bottom.width = px(3)
      ) %>%
      tab_source_note(md("<br>@bineneothniel.tra <br> "))
  }
  
}



# mod_train_test 

# après le rbind de train test, permet de verifier si les variables qualitative ont 
# les mêmes modalité dans train et test. et pour les variables pour lesquelles ce n'est pas le cas
# tracer des graphiques dont le nombre est le nb de var qui vérifie la cond

viz_train_test <- function(data, var) {
  var_name <- as_label(enquo(var))
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
    ggplot(aes(x = {{var}}, fill = type)) +
    geom_bar(position = "fill") +
    labs(
      title = paste("repartion des moadlités de ", var_name, " entre train et test"),
      x = var_name,
      y = ""
    ) +
    theme_minimal()
}
viz_train_test(data, MSSubClass)


var_type_viz <- function(data) {
  # nb_int <- sum(sapply(data, is.integer))
  nb_num <- sum(sapply(data, is.numeric))
  nb_char <- sum(sapply(data, is.character))
  nb_log <- sum(sapply(data, is.logical))
  nb_fact <- sum(sapply(data, is.factor))
  
  
  df <- data.frame(
    type = c("numeric", "character", "logical", "factor"),
    freq = c(nb_num, nb_char, nb_log, nb_fact)
  ) %>%
    arrange(desc(freq))
  
  df %>%
    hchart(type = "column",
           hcaes(x = "type", y = "freq"),
           color = "steelblue") %>%
    hc_title(
      text = "Bar Plots",
      style = list(fontWeight = "bold", fontSize = "30px"),
      align = "center"
    ) %>%
    hc_subtitle(
      text = "variables types",
      style = list(fontWeight = "bold"),
      align = "center"
    ) %>%
    hc_credits(
      enabled = TRUE,
      text = "md(Data Source: kaggle: https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques))",
      style = list(fontSize = "10px")
    ) %>%
    hc_xAxis(title = list(text = "type")) %>%
    hc_yAxis(title = list(text = "frequency")) %>%
    hc_add_theme(hc_theme_economist())
}
var_type_viz(train)


# Catégorielle ----

# ===
var_quali_viz_1 <- function(data, var) {
  var_name <- as_label(enquo(var))
  
  data <- data %>%
    count( {{var}}) %>%
    mutate(prop = n , label = paste0(round(prop, 1), "%"))
  
  data$text <- paste("Catégorie:", data[, 1], "<br>Valeur:", data[, 2])
  
  p  <- data %>%
    ggplot(mapping = aes( x = {{var}}, y = prop, text = text )) +
    geom_bar(stat = "identity", colour = lkp_blue,
             fill = lkp_blue) +
    labs(x = var_name, y = "Frequency") + custom_theme 
  
  ggplotly(p, tooltip = "text")  %>%
  config(displayModeBar = FALSE) %>%
    layout(margin = list(t = 0, b = 0, l = 0, r = 0))
}
var_quali_viz_1(train, MSZoning)



# ===
var_quali_viz_2 <- function(data, var) {
  var_name <- as_label(enquo(var))
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
    ggplot(mapping = aes(
      x = forcats::fct_infreq({{var}}),
      y = after_stat(prop),
      by = 1
    )) +
    geom_bar(colour = "#4477AA",
             fill = "#4477AA",
             stat = "prop") +
    labs(title = var_name, y = "Frequency") + theme_minimal() +
    
    theme(panel.grid = element_blank(), axis.text.y = element_blank()) +
    geom_text(aes(label = after_stat(prop) %>%
                    scales::percent(accuracy = .1)),
              stat = "prop",
              nudge_y = .03) + 
    theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = 0, face = "bold")) +
    
    scale_y_continuous(labels = scales::label_percent()) +
    xlab(NULL) + ylab(NULL) 
  
}
var_quali_viz_2(train, MSZoning)



# ===
var_quali_viz_3 <- function(data, class) {
  var_name <- as_label(enquo(class))
  
  data <- data %>%
    mutate(name = fct_infreq({{class}})) %>%
    count(name) %>%
    mutate(
      prop = n / sum(n) * 100, label = if_else(prop > 12, paste0(round(prop, 1), "%"), "")) %>%
    arrange(prop) %>%
    mutate()
  
  
  ggplot(data, aes(x = "", y = prop, fill = name)) +
    geom_bar(stat = "identity",
             width = 0.1,
             linewidth = 0.01,
             color = "white") +
    coord_polar(theta = "y", start = 0) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 6.5
    ) + theme_void() +
    labs(x = NULL, y = NULL) + scale_fill_brewer(palette = "Dark2")

  }
var_quali_viz_3(train, MSZoning)


# ===
R2_coef <- function(y_pred, y) {
  sst <- sum((y - mean(y)) ^ 2)
  sse <- sum((y_pred - y) ^ 2)
  rsq <- 1 - sse / sst
  return(rsq)
}
RMSE <- function(y_pred, y) {
  # Calcul de la somme des erreurs au carré
  sse <- sum((y_pred - y) ^ 2)
  # Calcul du nombre d'observations
  n <- length(y)
  # Calcul de la RMSE
  rmse <- sqrt(sse / n)
  return(rmse)
}
var_cible_quali_1 <- function(data, var, label) {
  var_name <- as_label(enquo(var))
  label_name <- as_label(enquo(label))
  
  data[[var_name]] <- as.character(data[[var_name]])
  n <- nrow(data)
  
  grouped_data <- data %>%
    group_by({{var}}) %>%
    summarise(moyenne = mean(!!sym(label_name), na.rm = TRUE))  # Calculate mean SalePrice for each group
  
  y <- rep(0, times = n)
  
  for (i in 1:n) {
    ind <- which(grouped_data[[1]] == data[[var_name]][i])
    y[i] <- grouped_data$moyenne[ind]
  }
  
  R2 <- R2_coef(y, data[[label_name]])
  RMSE <- RMSE(y, data[[label_name]])
  
  grouped_data %>%
    ggplot(mapping = aes(x = {{var}}, y = moyenne, )) +
    geom_col(fill = "#219C90", colour = "white", ) +
    labs(x = var_name,
         y = paste0("mean of ", label_name),
         title = "") +
    annotate(
      "richtext",
      x = Inf,
      y = Inf,
      label = glue(
        "<b><span style='color:blue'>R2      :</span></b> {round(R2, 3)} <br>
          <b><span style='color:blue'>RMSE      :</span></b> {round(RMSE, 3)} "
      ),
      hjust = 1.1,
      vjust = 1.1,
      fill = "#D8EFD3",
      label.color = NA,
      color = "black",
      size = 4
    ) + custom_theme
  
  # ggplotly(p) %>% config(displayModeBar = FALSE) %>%
  #layout(margin = list(t = 0, b = 0, l = 0, r = 0))
}
var_cible_quali_1(train, MSZoning, SalePrice)

# ===
var_cible_quali_2 <- function(data = train, var) {
  var_name <- as_label(enquo(var))
  
  data %>%
    ggplot(aes(x = {{var}} , y = SalePrice)) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_boxplot(
      fill = "dodgerblue1",
      colour = "black",
      alpha = 0.5,
      outlier.colour = "tomato2"
    ) +
    labs(x = var_name, y = "SalePrice") +
    theme(
      axis.title.x = element_text(
        size = 15,
        color = "blue",
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 10,
        color = "red",
        face = "italic"
      )
    )
}
var_cible_quali_2(data = train, MSZoning)


# ===
var_cible_quali_3 <- function(data = train, var) {
  var_name <- as_label(enquo(var))
  
  data %>%
    ggplot(aes(x = SalePrice, y = {{var}}, fill = {{var}})) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none") +
    labs(x = var_name, y = "SalePrice", title = "distribution de prix par zonne")
}
var_cible_quali_3(train,MSZoning)




# Numérique ----

# ===
var_quanti_viz_1 <- function(data, var) {
  var_name <- as_label(enquo(var))
  
  # Graphiques basiques
  gghistogram(
    data = data,
    x = var_name,
    fill = "steelblue",
    alpha = 1,
    bins = 15,
    add = "median",
    add_density = TRUE,
    rug = TRUE,
    title = paste("Histogram Plot of", var_name),
    xlab = var_name,
    ylab = "",
  )
}
var_quanti_viz_1(train, SalePrice)




library(rlang)
library(ggtext)
library(e1071) # or moments

# ===
var_quanti_viz_2 <- function(data, var, stat = FALSE, adjust = 0.8) {
  var_name <- as_label(enquo(var))
  
  # Check if the variable exists in the dataset
  if (!var_name %in% colnames(data)) {
    stop("The variable does not exist in the dataset")
  }
  
  # Calculate basic statistics
  min_value <- min(data[[var_name]], na.rm = TRUE)
  q1_value <- quantile(data[[var_name]], probs = 0.25, na.rm = TRUE)
  median_value <- quantile(data[[var_name]], probs = 0.5, na.rm = TRUE)
  mean_value <- mean(data[[var_name]], na.rm = TRUE)
  q3_value <- quantile(data[[var_name]], probs = 0.75, na.rm = TRUE)
  max_value <- max(data[[var_name]], na.rm = TRUE)
  
  # Additional statistics if stat is TRUE
  if (stat == TRUE) {
    skewness_value <- skewness(data[[var_name]], na.rm = TRUE)
    kurtosis_value <- kurtosis(data[[var_name]], na.rm = TRUE)
  }
  
  # Create the plot
  p <- ggplot(data = data, mapping = aes(x = !!enquo(var))) +
    geom_density(
      fill = "steelblue",
      alpha = 0.3,
      color = "lightblue",
      adjust = adjust
    ) +
    {
      if (stat == TRUE) {
        annotate(
          "richtext",
          x = Inf,
          y = Inf,
          label = glue::glue(
            "<b><span style='color:blue'>Mean    :</span></b> {round(mean_value, 2)} <br>
            <b><span style='color:red'>Median :</span></b> {round(median_value, 2)} <br>
            <b>Skewness:</b> {round(skewness_value, 2)} <br>
            <b>Kurtosis:</b> {round(kurtosis_value, 2)}"
          ),
          hjust = 1.1,
          vjust = 1.1,
          fill = "cornsilk",
          label.color = NA,
          color = "black",
          size = 4
        ) +
          geom_vline(
            aes(xintercept = median_value),
            color = "red",
            linetype = "dotted",
            linewidth = 1
          )
      } else {
        annotate(
          "richtext",
          x = Inf,
          y = Inf,
          label = glue::glue(
            "<b>Min    :</b> {round(min_value, 2)}  <br>
            <b>1st Qu    :</b> {round(q1_value, 2)}  <br>
            <b>Median    :</b> {round(median_value, 2)}  <br>
            <b><span style='color:blue'>Mean    :</span></b> {round(mean_value, 2)}  <br>
            <b>3rd Qu    :</b> {round(q3_value, 2)}  <br>
            <b>Max    :</b> {round(max_value, 2)}"
          ),
          hjust = 1.1,
          vjust = 1.1,
          fill = "cornsilk",
          label.color = NA,
          color = "black",
          size = 4
        )
      }
    } +
    geom_vline(
      aes(xintercept = mean_value),
      color = "blue",
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = paste("Density Plot of", var_name),
      x = var_name,
      y = "Density"
    ) +
    theme_minimal()
  
  p
}
var_quanti_viz_2(train, SalePrice, adjust = 1)

# ===
var_quanti_viz_3 <- function(data, var, n = 10) {
  var_name <- as_label(enquo(var))
  
  q1_value <- quantile(data[[var_name]], probs = 0.25, na.rm = TRUE)
  q3_value <- quantile(data[[var_name]], probs = 0.75, na.rm = TRUE)
  
  data <- data %>%
    mutate(
      nom = if_else(data[[var_name]] %in% sort(data[[var_name]], decreasing = TRUE)[1:n] |
                      (data[[var_name]] <= 1.5 * q1_value & data[[var_name]] >= 1.5 * q3_value), as.factor(Id), "")
    )
  
  ggplot(data, aes(x = factor(1), y = !!sym(var_name), label = nom)) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_text(size = 3, color = "black", vjust = -.75, fontface = "bold") +
    geom_boxplot(
      fill = "dodgerblue1",
      colour = "black",
      alpha = 0.7,
      outlier.colour = "tomato2",
      outlier.shape = 16,
      outlier.size = 2
    ) +
    labs(x = var_name, y = "") +
    theme(
      axis.title.x = element_text(
        size = 15,
        color = "blue",
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 10,
        color = "blue",
        face = "italic"
      )
    ) +
    theme_minimal()
}
var_quanti_viz_3(data = train, var = SalePrice, 1)




# ===
var_cible_quanti <- function(data, var) {
  var_name <- as_label(enquo(var))
  
  # Filtrer les données pour retirer les valeurs non finies
  data <- data %>%
    filter(!is.na(!!enquo(var)), !is.na(SalePrice))
  
  # Créer une formule de régression dynamiquement
  formula <- as.formula(paste("SalePrice ~", var_name))
  
  # Ajuster un modèle de régression linéaire
  model <- lm(formula, data = data)
  
  model_summary <- summary(model)
  coef <- coef(model)
  intercept <- coef[1]
  slope <- coef[2]
  r_squared <- model_summary$r.squared
  p_value <- coef(summary(model))[2, "Pr(>|t|)"]
  
  # Créer le texte d'annotation
  significance <- ifelse(p_value < 0.05, "Significatif", "Non significatif")
  annotation_text <- paste(
    sprintf("R² = %.2f", r_squared),
    sprintf("p-value = %.4f", p_value),
    sprintf("Coefficient = %.4f", slope),
    significance,
    sep = "\n"
  )

  # Créer un graphique avec ggplot2
  p <- ggplot(data, aes(x = !!enquo(var), y = SalePrice)) +
    geom_point(color = "black", size = 1.5) +                            # Ajouter des points
    geom_smooth(method = "lm",
                color = "red",
                se = FALSE) +
    labs(title = paste(var_name, " ~  SalePrice"),
         x = var_name,
         y = "SalePrice") +
    theme_classic() +
    theme(
      axis.title.x = element_text(
        size = 15,
        color = lkp_blue,
        # Assurez-vous de passer une couleur de type scalaire
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 10,
        color = "red",
        # Assurez-vous de passer une couleur de type scalaire
        face = "italic"
      ),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    annotate(
      "richtext",
      x = Inf,
      y = Inf,
      label = glue(
        "
R² = {sprintf('%.2f', r_squared)} <br>
p-value = {sprintf('%.4f', p_value)} <br>
Coefficient = {sprintf('%.4f', slope)} <br>
{significance}
"
      ),
      hjust = 1.1,
      vjust = 1.1,
      fill = "cornsilk",
      label.color = NA,
      color = "black",
      size = 4,
      fontface = "bold"
    )
  
  # Afficher le graphique
  print(p)
}
var_cible_quanti(data = train, LotArea)


