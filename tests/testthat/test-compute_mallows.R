cl <- parallel::makeCluster(2)
test_that("compute_mallows works with seed in parallel", {
  set.seed(1)
  mod <- compute_mallows(
    data = setup_rank_data(potato_visual),
    compute_options = set_compute_options(nmc = 10),
    cl = cl
  )
  expect_equal(mod$rho$value[[145]], 7)
  expect_equal(mod$rho$value[[89]], 14)
  expect_true(all(apply(mod$rho_samples, 3, validate_permutation)))
})

test_that("compute_mallows works with initial values and clusters", {
  set.seed(1)
  mod <- compute_mallows(
    setup_rank_data(potato_visual),
    model_options = set_model_options(n_clusters = 3),
    compute_options = set_compute_options(nmc = 10),
    initial_values = set_initial_values(alpha_init = c(2, 3)),
    cl = cl
  )
  expect_true(
    all(subset(mod$alpha, chain == 1 & iteration == 1)$value == 2)
  )
  expect_true(
    all(subset(mod$alpha, chain == 2 & iteration == 1)$value == 3)
  )
})
parallel::stopCluster(cl)

test_that("compute_mallows fails properly", {
  expect_error(
    compute_mallows(
      data = setup_rank_data(
        cbind(potato_visual, potato_visual + 20, potato_visual + 40)
      ),
      model_options = set_model_options(metric = "spearman")
    ),
    "Exact partition function not known."
  )

  expect_error(
    compute_mallows(data = potato_visual),
    "data must be an object of class BayesMallowsData"
  )

  prefs <- data.frame(
    assessor = 1, bottom_item = c(1, 2, 3), top_item = c(2, 1, 2)
  )
  expect_error(
    compute_mallows(setup_rank_data(preferences = prefs)),
    "Intransitive pairwise comparisons. Please specify an error model."
  )

  expect_error(
    compute_mallows(
      data = setup_rank_data(potato_visual),
      initial_values = set_initial_values(rho_init = rnorm(20))
    ),
    "rho_init must be a proper permutation"
  )

  expect_error(
    compute_mallows(
      data = setup_rank_data(potato_visual),
      initial_values = set_initial_values(rho_init = 1:3)
    ),
    "initial value for rho must have one value per item"
  )
})

test_that("compute_mallows is platform independent", {
  set.seed(1)
  mod <- compute_mallows(setup_rank_data(potato_visual))
  expect_equal(mod$alpha$value[1998], 10.2019196814125)

  dat <- potato_visual
  dat[dat > 5] <- NA
  mod <- compute_mallows(setup_rank_data(dat))
  expect_equal(mod$alpha$value[1998], 7.26333324707436)
})

test_that("compute_mallows rejects empty data", {
  expect_error(compute_mallows(
    data = setup_rank_data(rankings = matrix(nrow = 0, ncol = 10)),
    compute_options = set_compute_options(nmc = 10)
  ))
})
