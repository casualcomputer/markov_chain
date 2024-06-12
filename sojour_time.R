library(msm)

# Define the transition matrix Q
Q <- rbind(
  c(0, 0.25, 0, 0.25),
  c(0.166, 0, 0.166, 0.166),
  c(0, 0.25, 0, 0.25),
  c(0, 0, 0, 0)
)

# Fit the multi-state model
cav.msm <- msm(state ~ years, subject = PTNUM, data = cav, qmatrix = Q, deathexact = 4)

# Calculate sojourn times
sojourn_estimates <- sojourn.msm(cav.msm)
sojourn_time <- sojourn_estimates$estimates
sojourn_time_lower <- sojourn_estimates$L
sojourn_time_upper <- sojourn_estimates$U

# Add NA for the absorbing state
sojourn_time <- c(sojourn_time, NA)
sojourn_time_lower <- c(sojourn_time_lower, NA)
sojourn_time_upper <- c(sojourn_time_upper, NA)

# Summarize cav data and add sojourn times
library(dplyr)
cav.summary <- cav %>%
  group_by(state) %>%
  summarise(median_years = median(years), mean_years = mean(years)) %>%
  arrange(state) %>%
  mutate(sojourn_time = sojourn_time, 
         sojourn_time_lower = sojourn_time_lower, 
         sojourn_time_upper = sojourn_time_upper)

# Display the summary
print(cav.summary)

# Create a sample matrix Q
convert_to_matrix <- function(values, nrow) {
  if (length(values) %% nrow != 0) {
    stop("The number of values must be divisible by the number of rows.")
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
}

# Example usage:
values <- c(
  0, 0.25, 0, 0.25,
  0.166, 0, 0.166, 0.166,
  0, 0.25, 0, 0.25,
  0, 0, 0, 0
)

Q_matrix <- convert_to_matrix(values, 4)  # Automatically names states if not specified
print(Q_matrix)



Q_matrix <- convert_to_matrix(Q,nrow=nrow(Q))

# Save the matrix Q as an R object in a file named 'Q_matrix.RData'
save(Q_matrix, file = "Q_matrix.RData")