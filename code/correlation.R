library(reshape2)

# l'idéale ça doit être blanc, si c'est trop coloré, c'est pas bon

corelation_heatmap <- function(data) {
  numeric_train <- data %>% select_if(is.numeric)
  
  # Calculate the correlation matrix
  cor_matrix <- cor(numeric_train)
  
  # Reshape the correlation matrix
  melted_cor_matrix <- data.table::melt(cor_matrix)
  
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
    theme_minimal() +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 5,
      hjust = 1
    )) +
    coord_fixed() +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")
  
}
corelation_heatmap(train)


data <- iris
numerical_corelation <- function(data, threshold = 0.6) {
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
  connected_groups <- connected_vars %>%
    group_by(component) %>%
    summarise(variables = paste(variable, collapse = ", ")) %>%
    filter(sapply(strsplit(variables, "\\s+"), length) > 1)
  if (nrow(cor_matrix) > 15) {
    return(list(data = connected_groups))
  } else{
    graph1 <- plot(cor_graph)visNetwork(nodes,edges)
    
    graph2 <- pairs(data %>% dplyr::select(where(is.numeric)))
    
    return(list(
      data = connected_groups,
      graph1 = graph1,
      graph2 = graph2
    ))
  }
  
  
}
numerical_corelation(iris)


library(visNetwork)
cor_matrix <- cor(data %>% dplyr::select(where(is.numeric)))
adj_matrix <- abs(cor_matrix) > threshold
diag(adj_matrix) <- 0

# Create a graph from the adjacency matrix
cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Convert igraph object to visNetwork-compatible format
nodes <- data.frame(id = V(cor_graph)$name, 
                    label = V(cor_graph)$name, 
                    title = V(cor_graph)$name)
edges <- data.frame(from = as.character(ends(cor_graph, E(cor_graph))[,1]), 
                    to = as.character(ends(cor_graph, E(cor_graph))[,2]))

# Create the visNetwork visualization

visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visInteraction(navigationButtons = TRUE)


corelation_network <- function(data, threshold = 0.8){
  
  # Calculate correlation and adjacency matrices
  cor_matrix <- cor(data %>% dplyr::select(where(is.numeric)))
  adj_matrix <- abs(cor_matrix) > threshold
  diag(adj_matrix) <- 0
  
  # Create a graph from the adjacency matrix
  cor_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  
  # Get the degree of each node
  node_degrees <- degree(cor_graph)
  
  # Create nodes data frame
  nodes <- data.frame(
    id = V(cor_graph)$name,
    label = V(cor_graph)$name,
    group = ifelse(node_degrees > 1, "connected", "isolated"),  # Assign group based on degree
    title = V(cor_graph)$name,
    size = 10,
    color = ifelse(node_degrees > 1, "red", "grey")  # Color for connected nodes, default for isolated
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
      highlightNearest = list(enabled = TRUE, degree = 10),  # Highlight nearest nodes
      nodesIdSelection = TRUE  # Enable node ID selection
    ) %>%
    visInteraction(
      dragNodes = TRUE,  # Enable dragging of nodes
      dragView = TRUE,  # Enable dragging of the view
      zoomView = TRUE  # Enable zooming
    ) %>%
    visLayout(randomSeed = 42)  # Ensure consistent layout
}

