reshape_vector_to_matrix <- function(vector, nrow, ncol) {
  if (length(vector) != nrow * ncol) {
    stop("The length of the vector does not match the specified dimensions.")
  }
  # Create the matrix and then transpose it
  matrix_transposed <- t(matrix(vector, nrow = nrow, ncol = ncol, byrow = TRUE))
  return(matrix_transposed)
}


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
