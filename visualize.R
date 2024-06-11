library(visNetwork)

# Function to create and plot an interactive graph with highly readable labels
create_and_plot_interactive_graph <- function(Q, state_names = NULL) {
  # Check if state names are provided, else create default ones
  if (is.null(state_names)) {
    num_states <- nrow(Q)
    state_names <- paste("S", 1:num_states, sep = "")
  }
  
  # Validate the length of state_names matches the number of states in Q
  if (length(state_names) != nrow(Q)) {
    stop("The length of state_names does not match the number of states in Q.")
  }
  
  # Create transitions data frame
  transitions <- expand.grid(from = state_names, to = state_names)
  transitions$weight <- as.vector(Q)
  transitions <- transitions[transitions$weight > 0, ]
  
  # Nodes and edges data frames
  nodes <- data.frame(id = state_names, label = state_names, font = list(face = "arial", color = "white"), color = list(background = "cornflowerblue", border = "darkblue"))
  edges <- data.frame(from = transitions$from, to = transitions$to, label = sprintf("%.2f", transitions$weight), arrows = 'to')
  
  # Create the network
  net <- visNetwork(nodes, edges, width = "100%", height = "600px") %>%
    visNodes(shape = "circle") %>%
    visEdges(smooth = list(type = "dynamic", roundness = 0.5), font = list(align = "top")) %>%
    visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -100, centralGravity = 0.01, springLength = 100, springConstant = 0.05))
  
  # Optional features
  if ("navigationButtons" %in% names(formals(visOptions))) {
    net <- net %>% visOptions(navigationButtons = TRUE)
  }
  if ("highlightNearest" %in% names(formals(visOptions))) {
    net <- net %>% visOptions(highlightNearest = TRUE)
  }
  
  return(net)
}

# Example usage
Q_6 <- matrix(runif(36, 0, 0.3), 6, 6)  # Random probabilities for demonstration
rownames(Q_6) <- colnames(Q_6) <- paste("State", 1:6)
custom_state_names <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta")
g_interactive <- create_and_plot_interactive_graph(Q_6, custom_state_names)
g_interactive
