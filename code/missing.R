
# missing_viz
missing_viz <- function(data, class) {
  var_name <- as_label(enquo(class))
  
  data <- data %>%
    dplyr::select({{class}}) %>%
    mutate(observation = ifelse(is.na({{class}}), "missing", "value"))
  
  data <- data %>%
    count(observation) %>%
    mutate(prop = n / sum(n) * 100, label = paste0(round(prop, 1), "%"))
  
  ggplot(data, aes(x = 2, y = prop, fill = observation)) +
    geom_bar(stat = "identity",
             width = 1,
             color = "white") +
    coord_polar(theta = "y", start = 0) +
    scale_fill_manual(values = c(lkp_magenta, "#E8C5E5"), guide = FALSE)  +
    scale_color_brewer(palette = "Dark2") +
    labs(x = NULL, y = NULL) +
    theme_void() +
    xlim(0.5, 2.5) +
    ggtitle(paste0(var_name, round( data$prop[1], 1), " Missing Value")) +
  ggtitle(paste0( round(data$prop[1], 1))) +
    labs(subtitle = var_name) +
    theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0, face = "bold")) +
    theme(plot.subtitle = element_text(size = 14, hjust = 0.5, vjust = 0))
  
}


missing_viz(data = train, class = LotFrontage )


missing_table <- function(data, n) {
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter(missing > 0)
  
  miss_data %>%
    gt_trabi(n)
}
missing_table(train, 8)



# nécessaire pour observer les variables qui ont des valeurs manquantes corrélé et peu nous guider dans la compréhension de l'abscence des valeurs manquantes

missing_heatmap <- function(data) {
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter(missing > 0)
  
  missing <- miss_data %>% distinct(variables) %>% pull(variables)
  
  # Convert data to a matrix
  data_matrix <- as.matrix(data %>% dplyr::select(all_of(missing)))
  
  # Create a logical matrix indicating missing values
  missing_data <- is.na(data_matrix)
  
  # Define a color palette
  col_fun <- colorRamp2(c(FALSE, TRUE), c("lightblue", "tomato"))
  
  # Create the heatmap
  Heatmap(
    missing_data,
    height = 600,
    show_heatmap_legend = FALSE,
    name = "Missing Values",
    col = col_fun,
    show_row_names = TRUE,
    show_column_names = TRUE,
    column_names_side = "top",
    column_names_gp = gpar(fontsize = 10),
    row_names_gp = gpar(fontsize = 10),
    heatmap_legend_param = list(
      title = "Missing",
      at = c(FALSE, TRUE),
      labels = c("value", "missing")
    ),
    cluster_rows = FALSE,
    cluster_columns = FALSE
  )
}

# missing_heatmap(train)


# repartition of missing
missing_loliplot <- function(data) {
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter(missing > 0)
  
  
  missing <- miss_data %>% distinct(variables) %>% as.vector()
  gg_miss_var(data %>% dplyr::select(missing$variables))
}

missing_loliplot(train)
