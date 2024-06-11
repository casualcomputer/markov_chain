library(visNetwork)

# Function to map weights to colors and calculate widths
map_weight_to_color <- function(weight, max_weight) {
  # Normalize weight to a 0-1 scale based on maximum weight
  normalized_weight <- weight / max_weight
  # Convert to a scale of 255 (for RGB color scaling)
  color_intensity <- as.integer(255 * (1 - normalized_weight))
  # Create an RGB color, darker for higher weights
  sprintf("#%02x%02x%02x", color_intensity, color_intensity, 255)
}

create_and_plot_interactive_graph <- function(Q, state_names = NULL) {
  if (is.null(state_names)) {
    num_states <- nrow(Q)
    state_names <- paste("S", 1:num_states, sep = "")
  }
  
  transitions <- expand.grid(from = state_names, to = state_names)
  transitions$weight <- as.vector(Q)
  transitions <- transitions[transitions$weight > 0, ]
  
  # Apply the color mapping and calculate width
  max_weight <- max(transitions$weight)
  transitions$color <- sapply(transitions$weight, map_weight_to_color, max_weight)
  transitions$width <- (transitions$weight / max_weight) * 5 + 1  # Scale from 1 to 10
  
  nodes <- data.frame(id = state_names, label = state_names, font = list(face = "arial", color = "white"), color = list(background = "cornflowerblue", border = "darkblue"))
  edges <- data.frame(
    from = transitions$from, 
    to = transitions$to, 
    label = sprintf("%.2f", transitions$weight), 
    arrows = 'to', 
    color = transitions$color,
    width = transitions$width
  )
  
  net <- visNetwork(nodes, edges, width = "100%", height = "600px") %>%
    visNodes(shape = "circle") %>%
    visEdges(smooth = list(type = "dynamic", roundness = 0.5), font = list(align = "top")) %>%
    visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -100, centralGravity = 0.01, springLength = 100, springConstant = 0.05),
               stabilization = list(enabled = TRUE, iterations = 30)  # Enable stabilization
    )
  
  return(net)
}
# 
# # Example usage
# Q_6 <- matrix(runif(36, 0, 0.3), 6, 6)  # Random probabilities for demonstration
# rownames(Q_6) <- colnames(Q_6) <- paste("State", 1:6)
# custom_state_names <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta")
# g_interactive <- create_and_plot_interactive_graph(Q_6, custom_state_names)
# g_interactive
