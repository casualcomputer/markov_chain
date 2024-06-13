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

# Function to create and plot an interactive graph
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
    visNodes(shape = "circle", value = 20) %>%
    visEdges(arrows = 'to', width = 2, arrowStrikethrough = TRUE, smooth = list(smooth = list(enabled = FALSE)), font = list(align = "top")) %>%
    visIgraphLayout(layout = "layout_in_circle") %>%  # This layout will be fixed
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01,
        springLength = 200,
        springConstant = 0.05,
        avoidOverlap = 1
      )
    ) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE)
  
  return(net)
}

# Function to reshape a vector into a matrix
reshape_vector_to_matrix <- function(vector, nrow, ncol) {
  if (length(vector) != nrow * ncol) {
    stop("The length of the vector does not match the specified dimensions.")
  }
  # Create the matrix and then transpose it
  matrix_transposed <- t(matrix(vector, nrow = nrow, ncol = ncol, byrow = TRUE))
  return(matrix_transposed)
}

# Function to convert a vector of values into a matrix with state names
convert_to_matrix <- function(values, nrow, state_names = NULL) {
  if (length(values) %% nrow != 0) {
    stop("The number of values must be divisible by the number of rows.")
  }
  
  # Default state names if not provided
  if (is.null(state_names)) {
    state_names <- paste("State", 1:nrow)
  }
  
  # Create the matrix
  mat <- matrix(values, nrow = nrow, byrow = TRUE)
  
  # Set dimension names
  dimnames(mat) <- list(state_names, state_names)
  
  return(mat)
}

# # How I set up the "Q_matrix.RData":
# Q_matrix = as.matrix(rbind(
#   c(0, 0.25, 0, 0.25),
#   c(0.166, 0, 0.166, 0.166),
#   c(0, 0.25, 0, 0.25),
#   c(0, 0, 0, 0)
# ))
# save(Q_matrix, file = "Q_matrix.RData")
