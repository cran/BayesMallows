# Create example data. We set the burn-in and thinning very low
# for the sampling to go fast
data0 <- sample_mallows(rho0 = 1:5, alpha = 10, n_samples = 1000,
                        burnin = 10, thinning = 1)
# Find the frequency distribution
compute_observation_frequency(rankings = data0)

# The function also works when the data have missing values
rankings <- matrix(c(1, 2, 3, 4,
                     1, 2, 4, NA,
                     1, 2, 4, NA,
                     3, 2, 1, 4,
                     NA, NA, 2, 1,
                     NA, NA, 2, 1,
                     NA, NA, 2, 1,
                     2, NA, 1, NA), ncol = 4, byrow = TRUE)

compute_observation_frequency(rankings)
