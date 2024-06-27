library(reshape2)

# l'idéale ça doit être blanc, si c'est trop coloré, c'est pas bon


num_corelation_heatmap <- function(data, upper = TRUE) {
  numeric_train <- data %>% select_if(is.numeric)
  
  # Calculate the correlation matrix
  cor_matrix <- cor(numeric_train)
  
  diag(cor_matrix) <- NA
  # Select upper or lower triangle
  if (upper) {
    cor_matrix[lower.tri(cor_matrix)] <- NA
  } else {
    cor_matrix[upper.tri(cor_matrix)] <- NA
  }
  
  # Reshape the correlation matrix
  melted_cor_matrix <- data.table::melt(cor_matrix, na.rm = TRUE)
  
  # Create the heatmap
  ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = "Correlation"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 5,
      hjust = 1
    )) +
    coord_fixed() +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")
}
num_corelation_heatmap(train, upper = TRUE)



num_corelation_data <- function(data, threshold = 0.6) {
  cor_matrix <- cor(data %>% dplyr::select(where(is.numeric)))
  adj_matrix <- abs(cor_matrix) > threshold
  diag(adj_matrix) <- 0
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  
  # Identify connected components
  components <- components(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create a data frame to list the variables in each connected component
  connected_vars <- data.frame(variable = names(membership), component = membership)
  
  # Group and print the connected variables
  data <- connected_vars %>%
    group_by(component) %>%
    summarise(variables = paste(variable, collapse = ", ")) %>%
    filter(sapply(strsplit(variables, "\\s+"), length) > 1)
  
  
  data
}
num_corelation_data(train, 0.8)



num_corelation_network <- function(data, threshold = 0.6){
  
  # Load necessary libraries
  library(dplyr)
  library(igraph)
  library(visNetwork)
  
  # Calculate correlation and adjacency matrices
  cor_matrix <- cor(data %>% dplyr::select(where(is.numeric)), use = "pairwise.complete.obs")
  adj_matrix <- abs(cor_matrix) > threshold
  diag(adj_matrix) <- 0
  
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  
  # Get the degree of each node
  node_degrees <- degree(cor_graph)
  components <- clusters(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create nodes data frame
  nodes <- data.frame(
    id = V(cor_graph)$name,
    label = V(cor_graph)$name,
    group = ifelse(components$csize[membership] > 1, "connected", "isolated"),  # Assign group based on component size
    title = V(cor_graph)$name,
    size = 10,
    color = ifelse(components$csize[membership] > 1, "red", "gray")  # Color for connected nodes, gray for isolated
  )
  
  # Create edges data frame
  edges <- data.frame(
    from = as.character(ends(cor_graph, E(cor_graph))[,1]), 
    to = as.character(ends(cor_graph, E(cor_graph))[,2]),
    width = 2
  )
  
  # Create the visNetwork visualization with customizations
  visNetwork(nodes, edges) %>%
    visNodes(
      shape = "dot",  # Node shape
      scaling = list(min = 10, max = 30),  # Scaling size of nodes
      font = list(size = 20, face = "Arial"),  # Font style
      borderWidth = 2
    ) %>%
    visEdges(
      color = list(color = "gray", highlight = "red"),  # Edge color
      smooth = FALSE,  # Disable smooth edges
      width = 2
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 6),  # Highlight nearest nodes
      nodesIdSelection = TRUE  # Enable node ID selection
    ) %>%
    visInteraction(
      dragNodes = TRUE,  # Enable dragging of nodes
      dragView = TRUE,  # Enable dragging of the view
      zoomView = TRUE  # Enable zooming
    ) %>%
    visLayout(randomSeed = 42)  # Ensure consistent layout
}
num_corelation_network(train)



library(vcd)

#  V de Cramer
v_cramers <- function(x, y) {
  chi2 <- chisq.test(table(x, y))
  n <- sum(table(x, y))
  phi2 <- chi2$statistic / n
  r <- nrow(table(x, y))
  k <- ncol(table(x, y))
  v <- sqrt(phi2 / min(r - 1, k - 1))
  return(v)
}

cat_corelation_heatmap <- function(data, upper = TRUE) {
  cat_vars <- data %>% dplyr::select(where(is.factor) | where(is.character))
  
  # Initialiser une matrice pour stocker les coefficients V de Cramer
  n <- ncol(cat_vars)
  v_matrix <- matrix(0, n, n)
  rownames(v_matrix) <- colnames(cat_vars)
  colnames(v_matrix) <- colnames(cat_vars)
  
  # Calculer V de Cramer pour chaque paire de variables
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      tbl <- table(cat_vars[[i]], cat_vars[[j]])
      v_cramer <- assocstats(tbl)$cramer
      v_matrix[i, j] <- v_cramer
      v_matrix[j, i] <- v_cramer
    }
  }
  
  diag(v_matrix) <- NA
  # Select upper or lower triangle
  if (upper) {
    v_matrix[lower.tri(v_matrix)] <- NA
  } else {
    v_matrix[upper.tri(v_matrix)] <- NA
  }
  
  # Reshape the correlation matrix
  melted_cor_matrix <- reshape2::melt(v_matrix, na.rm = TRUE)
  
  # Create the heatmap
  ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = "Correlation"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 5,
      hjust = 1
    )) +
    coord_fixed() +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")
  
}
cat_corelation_heatmap(data = train)



cat_corelation_data <- function(data, threshold = 0.7) {
  
  cat_vars <- data %>% dplyr::select(where(is.factor) | where(is.character))
  
  # Initialiser une matrice pour stocker les coefficients V de Cramer
  n <- ncol(cat_vars)
  v_matrix <- matrix(0, n, n)
  rownames(v_matrix) <- colnames(cat_vars)
  colnames(v_matrix) <- colnames(cat_vars)
  
  # Calculer V de Cramer pour chaque paire de variables
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      tbl <- table(cat_vars[[i]], cat_vars[[j]])
      v_cramer <- assocstats(tbl)$cramer
      v_matrix[i, j] <- v_cramer
      v_matrix[j, i] <- v_cramer
    }
  }
  
  adj_matrix <- abs(v_matrix) > threshold
  diag(adj_matrix) <- 0
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  
  components <- components(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create a data frame to list the variables in each connected component
  connected_vars <- data.frame(variable = names(membership), component = membership)
  
  # Group and print the connected variables
  data <- connected_vars %>%
    group_by(component) %>%
    summarise(variables = paste(variable, collapse = ", ")) %>%
    filter(sapply(strsplit(variables, "\\s+"), length) > 1)
  
  data
}
cat_corelation_data(data = train, 0.7)

cat_corelation_network <- function(data, threshold = 0.7) {
  
  cat_vars <- data %>% dplyr::select(where(is.factor) | where(is.character))
  
  # Initialiser une matrice pour stocker les coefficients V de Cramer
  n <- ncol(cat_vars)
  v_matrix <- matrix(0, n, n)
  rownames(v_matrix) <- colnames(cat_vars)
  colnames(v_matrix) <- colnames(cat_vars)
  
  # Calculer V de Cramer pour chaque paire de variables
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      tbl <- table(cat_vars[[i]], cat_vars[[j]])
      v_cramer <- assocstats(tbl)$cramer
      v_matrix[i, j] <- v_cramer
      v_matrix[j, i] <- v_cramer
    }
  }
  
  adj_matrix <- abs(v_matrix) > threshold
  diag(adj_matrix) <- 0
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  # Get the degree of each node
  node_degrees <- degree(cor_graph)
  components <- clusters(cor_graph)
  
  # Extract the membership vector indicating component membership for each variable
  membership <- components$membership
  
  # Create nodes data frame
  nodes <- data.frame(
    id = V(cor_graph)$name,
    label = V(cor_graph)$name,
    group = ifelse(components$csize[membership] > 1, "connected", "isolated"),  # Assign group based on component size
    title = V(cor_graph)$name,
    size = 10,
    color = ifelse(components$csize[membership] > 1, "red", "gray")  # Color for connected nodes, gray for isolated
  )
  
  # Create edges data frame
  edges <- data.frame(
    from = as.character(ends(cor_graph, E(cor_graph))[,1]), 
    to = as.character(ends(cor_graph, E(cor_graph))[,2]),
    width = 2
  )
  
  # Create the visNetwork visualization with customizations
  visNetwork(nodes, edges) %>%
    visNodes(
      shape = "dot",  # Node shape
      scaling = list(min = 10, max = 30),  # Scaling size of nodes
      font = list(size = 20, face = "Arial"),  # Font style
      borderWidth = 2
    ) %>%
    visEdges(
      color = list(color = "gray", highlight = "red"),  # Edge color
      smooth = FALSE,  # Disable smooth edges
      width = 2
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 6),  # Highlight nearest nodes
      nodesIdSelection = TRUE  # Enable node ID selection
    ) %>%
    visInteraction(
      dragNodes = TRUE,  # Enable dragging of nodes
      dragView = TRUE,  # Enable dragging of the view
      zoomView = TRUE  # Enable zooming
    ) %>%
    visLayout(randomSeed = 42)  # Ensure consistent layout
}
cat_corelation_network(train, 0.6 )

