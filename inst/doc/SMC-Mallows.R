## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----sushi_rankings_demo1, message=FALSE, warning=FALSE-----------------------
library(BayesMallows)
library(dplyr)
library(ggplot2)

## ----sushi_rankings_demo------------------------------------------------------
head(sushi_rankings)

## ----set_seed, message=FALSE, warning=FALSE, include=FALSE--------------------
set.seed(994)

## ----smc_complete_set_up------------------------------------------------------
n_items <- dim(sushi_rankings)[2]
metric <- "footrule"
alpha_vector <- seq(from = 0, to = 15, by = 0.1)
iter <- 1e4
degree <- 10
# Estimate the logarithm of the partition function of the Mallows rank model
logz_estimate <- estimate_partition_function(
  method = "importance_sampling",
  alpha_vector = alpha_vector,
  n_items = n_items, metric = metric,
  nmc = iter, degree = degree
)
data <- sushi_rankings[1:100, ]
leap_size <- floor(n_items / 5)
N <- 1000
Time <- 20
smc_test <- smc_mallows_new_users_complete(
  R_obs = data, n_items = n_items,
  metric = metric, leap_size = leap_size,
  N = N, Time = Time,
  logz_estimate = logz_estimate,
  mcmc_kernel_app = 5,
  num_new_obs = 5,
  alpha_prop_sd = 0.5,
  lambda = 0.15,
  alpha_max = 1e6
)

## ----smc_complete_analysis_heatplot, message=FALSE, warning=FALSE-------------
test_sample_rho <- smc_test$rho_samples[, , Time + 1]
plot_rho_posterior(
  output = test_sample_rho,
  nmc = N, burnin = 0, C = 1,
  colnames = colnames(sushi_rankings)
)

## ----posterior_intervals_rho, message=FALSE, warning=FALSE--------------------
test_sample_rho <- smc_test$rho_samples[, , Time + 1]
compute_posterior_intervals_rho(
  output = test_sample_rho,
  nmc = N, burnin = 0,
  verbose = FALSE
)

## ----consensus_ranking_estimates, message=FALSE, warning=FALSE----------------
compute_rho_consensus(
  output = test_sample_rho, nmc = N,
  burnin = 0, C = 1, type = "CP",
  verbose = FALSE
)

## ----smc_complete_alpha_analysis, message=FALSE, warning=FALSE----------------
test_sample_alpha <- smc_test$alpha_samples[, Time + 1]
plot_alpha_posterior(
  output = test_sample_alpha, nmc = N,
  burnin = 0
)
compute_posterior_intervals_alpha(
  output = test_sample_alpha,
  nmc = N, burnin = 0, verbose = FALSE
)

## ----smc_partial_set_up-------------------------------------------------------
data_partial <- sushi_rankings[1:100, ]
data_partial[data_partial > 5] <- NA
head(data_partial)

## ----smc_partial_error, eval=FALSE, error=TRUE--------------------------------
#  aug_method <- "pseudolikelihood"
#  metric <- "cayley"
#  # example of selecting the incorrect combination of metric and aug_method
#  smc_partial_test <- smc_mallows_new_users_partial(
#    R_obs = data_partial,
#    n_items = n_items,
#    metric = metric,
#    leap_size = leap_size, N = N,
#    Time = Time,
#    logz_estimate = logz_estimate,
#    mcmc_kernel_app = 5,
#    num_new_obs = 5,
#    alpha_prop_sd = 0.5,
#    lambda = 0.15,
#    alpha_max = 1e6,
#    aug_method = aug_method
#  )
#  #>Error in smc_mallows_new_users_partial(R_obs = data_partial, n_items = n_items,
#  #>: Combined choice of metric and aug_method is incompatible

## ----set_seed2, message=FALSE, warning=FALSE, include=FALSE-------------------
set.seed(994)

## ----smc_partial_test---------------------------------------------------------
# aug_method = "random"
aug_method <- "pseudolikelihood"
metric <- "footrule"
smc_partial_test <- smc_mallows_new_users_partial(
  R_obs = data_partial,
  n_items = n_items,
  metric = metric,
  leap_size = leap_size, N = N,
  Time = Time,
  logz_estimate = logz_estimate,
  mcmc_kernel_app = 5,
  num_new_obs = 5,
  alpha_prop_sd = 0.5,
  lambda = 0.15,
  alpha_max = 1e6,
  aug_method = aug_method
)

## ----smc_partial_analysis, message=FALSE, warning=FALSE-----------------------
partial_test_sample_rho <- smc_partial_test$rho_samples[, , Time + 1]
partial_test_sample_alpha <- smc_partial_test$alpha_samples[, Time + 1]
plot_rho_posterior(
  output = partial_test_sample_rho, nmc = N,
  burnin = 0, C = 1, colnames = colnames(sushi_rankings)
)
plot_alpha_posterior(
  output = partial_test_sample_alpha, nmc = N,
  burnin = 0
)

## ----smc_item_rank_set_up, message=FALSE, warning=FALSE-----------------------
example_dataset <- potato_visual
n_users <- 12
n_items <- 20
test_dataset <- array(0, c(n_users, n_items, (n_items / 2 + 1)))
test_dataset[, , (n_items / 2 + 1)] <- potato_visual
tt <- 0
for (ii in (n_items - 1):(n_items / 2)) {
  tt <- tt + 1

  # set n_users line with one more NA
  example_dataset[example_dataset > ii] <- NA

  # set as new time stamp
  test_dataset[, , ((n_items / 2 + 1) - tt)] <- example_dataset
}

## ----test_dataset_example, message=FALSE, warning=FALSE-----------------------
test_dataset[, , 5]

## ----set_seed3, message=FALSE, warning=FALSE, include=FALSE-------------------
set.seed(995)

## ----new_item_rank_example, message=FALSE, warning=FALSE----------------------
# Recalculate the estimate of the partition function for 20 items
logz_estimate <- estimate_partition_function(
  method = "importance_sampling",
  alpha_vector = alpha_vector,
  n_items = n_items, metric = metric,
  nmc = iter, degree = degree
)
Time <- dim(test_dataset)[3]
N <- 1000
aug_method <- "pseudolikelihood"
metric <- "footrule"
smc_test_updated_partial <- smc_mallows_new_item_rank(
  n_items = n_items,
  R_obs = test_dataset,
  metric = metric,
  leap_size = leap_size, N = N,
  Time = Time,
  logz_estimate = logz_estimate,
  mcmc_kernel_app = 5,
  alpha_prop_sd = 0.5,
  lambda = 0.15,
  alpha_max = 1e6,
  aug_method = aug_method
)

## ----smc_updated_partial_analysis, message=FALSE, warning=FALSE---------------
updated_partial_test_sample_rho <- smc_test_updated_partial$rho_samples[, , Time]
updated_partial_test_sample_alpha <- smc_test_updated_partial$alpha_samples[, Time]
plot_rho_posterior(output = updated_partial_test_sample_rho, nmc = N, burnin = 0, C = 1)
plot_alpha_posterior(output = updated_partial_test_sample_alpha, nmc = N, burnin = 0)

