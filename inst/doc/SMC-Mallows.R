## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(BayesMallows)

## ----sushi_rankings_demo------------------------------------------------------
head(sushi_rankings)

## ----set_seed, message=FALSE, warning=FALSE, include=FALSE--------------------
set.seed(994)

## ----smc_complete_set_up------------------------------------------------------
n_items <- ncol(sushi_rankings)
metric <- "footrule"

logz_list <- prepare_partition_function(metric = metric, n_items = n_items)

data <- sushi_rankings[1:100, ]
leap_size <- floor(n_items / 5)
N <- 1000
Time <- 20
smc_test <- smc_mallows_new_users(
  R_obs = data, type = "complete", n_items = n_items,
  metric = metric, leap_size = leap_size,
  N = N, Time = Time,
  logz_estimate = logz_list$logz_estimate,
  cardinalities = logz_list$cardinalities,
  mcmc_kernel_app = 5,
  num_new_obs = 5,
  alpha_prop_sd = 0.5,
  lambda = 0.15,
  alpha_max = 1e6
)

## ----smc_complete_analysis_heatplot, message=FALSE, warning=FALSE-------------
plot(smc_test, colnames = colnames(sushi_rankings), parameter = "rho")

## ----posterior_intervals_rho, message=FALSE, warning=FALSE--------------------
compute_posterior_intervals(smc_test, parameter = "rho")

## -----------------------------------------------------------------------------
compute_consensus(smc_test)

## ----smc_complete_alpha_analysis, message=FALSE, warning=FALSE----------------
plot(smc_test, nmc = N, burnin = 0, parameter = "alpha")
compute_posterior_intervals(smc_test, parameter = "alpha")

## ----smc_partial_set_up-------------------------------------------------------
data_partial <- sushi_rankings[1:100, ]
data_partial[data_partial > 5] <- NA
head(data_partial)

## ----smc_partial_error, eval=FALSE, error=TRUE--------------------------------
#  aug_method <- "pseudolikelihood"
#  metric <- "cayley"
#  # example of selecting the incorrect combination of metric and aug_method
#  smc_partial_test <- smc_mallows_new_users(
#    R_obs = data_partial,
#    type = "partial",
#    n_items = n_items,
#    metric = metric,
#    leap_size = leap_size, N = N,
#    Time = Time,
#    logz_estimate = logz_list$logz_estimate,
#    cardinalities = logz_list$cardinalities,
#    mcmc_kernel_app = 5,
#    num_new_obs = 5,
#    alpha_prop_sd = 0.5,
#    lambda = 0.15,
#    alpha_max = 1e6,
#    aug_method = aug_method
#  )

## ----set_seed2, message=FALSE, warning=FALSE, include=FALSE-------------------
set.seed(994)

## ----smc_partial_test---------------------------------------------------------
aug_method <- "pseudolikelihood"
metric <- "footrule"
smc_partial_test <- smc_mallows_new_users(
  R_obs = data_partial,
  type = "partial",
  n_items = n_items,
  metric = metric,
  leap_size = leap_size, N = N,
  Time = Time,
  logz_estimate = logz_list$logz_estimate,
  cardinalities = logz_list$cardinalities,
  mcmc_kernel_app = 5,
  num_new_obs = 5,
  alpha_prop_sd = 0.5,
  lambda = 0.15,
  alpha_max = 1e6,
  aug_method = aug_method
)

## ----smc_partial_analysis, message=FALSE, warning=FALSE-----------------------
plot(smc_partial_test, colnames = colnames(sushi_rankings), parameter = "rho")
plot(smc_partial_test, nmc = N, burnin = 0, parameter = "alpha")

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
logz_list <- prepare_partition_function(metric = metric, n_items = n_items)

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
  logz_estimate = NULL,
  cardinalities = logz_list$cardinalities,
  mcmc_kernel_app = 5,
  alpha_prop_sd = 0.5,
  lambda = 0.15,
  alpha_max = 1e6,
  aug_method = aug_method
)

## ----smc_updated_partial_analysis, message=FALSE, warning=FALSE---------------
plot(smc_test_updated_partial, parameter = "rho", items = c(4, 6, 7))
plot(smc_test_updated_partial, parameter = "alpha")

