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
