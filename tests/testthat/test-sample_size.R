test_that("calculate_sample_size works with basic input", {
  fixed_fx <- c(intercept = 5, effect1 = 0.8, effect2 = 0.5, interaction = 0.6)
  random_fx <- c(subj_intercept = 1.0, subj_slope = 0.5)
  
  result <- calculate_sample_size(
    target_power = 0.80,
    effect_name = "effect1",
    n_obs_per_subj = 15,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_subj_range = c(15, 30),
    n_sim = 10,  # Small for testing
    seed = 321,
    verbose = FALSE
  )
  
  expect_s3_class(result, "pwrSim_sample_size")
  expect_true("target_power" %in% names(result))
  expect_true("recommended_n" %in% names(result))
  expect_true("results" %in% names(result))
  expect_equal(result$target_power, 0.80)
  expect_equal(result$effect_name, "effect1")
})


test_that("calculate_sample_size validates input", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5)
  random_fx <- c(subj_intercept = 1.0)
  
  # Invalid target_power
  expect_error(
    calculate_sample_size(target_power = 1.5, effect_name = "effect1",
                         n_obs_per_subj = 10, fixed_effects = fixed_fx,
                         random_effects = random_fx),
    "target_power must be between 0 and 1"
  )
  
  # Effect not in fixed_effects
  expect_error(
    calculate_sample_size(target_power = 0.8, effect_name = "nonexistent",
                         n_obs_per_subj = 10, fixed_effects = fixed_fx,
                         random_effects = random_fx),
    "effect_name 'nonexistent' not found in fixed_effects"
  )
  
  # Invalid n_subj_range
  expect_error(
    calculate_sample_size(target_power = 0.8, effect_name = "effect1",
                         n_obs_per_subj = 10, fixed_effects = fixed_fx,
                         random_effects = random_fx, n_subj_range = c(50, 20)),
    "n_subj_range must be a vector of length 2 with increasing values"
  )
})


test_that("print method for sample_size works", {
  fixed_fx <- c(intercept = 5, effect1 = 1.0)
  random_fx <- c(subj_intercept = 1.0)
  
  result <- calculate_sample_size(
    target_power = 0.80,
    effect_name = "effect1",
    n_obs_per_subj = 15,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_subj_range = c(10, 25),
    n_sim = 5,
    seed = 654,
    verbose = FALSE
  )
  
  expect_output(print(result), "Sample Size Calculation Results")
  expect_output(print(result), "Target power")
})


test_that("results data frame has correct structure", {
  fixed_fx <- c(intercept = 5, effect1 = 1.0)
  random_fx <- c(subj_intercept = 1.0)
  
  result <- calculate_sample_size(
    target_power = 0.80,
    effect_name = "effect1",
    n_obs_per_subj = 15,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_subj_range = c(10, 20),
    n_sim = 5,
    seed = 987,
    verbose = FALSE
  )
  
  expect_true(is.data.frame(result$results))
  expect_true("n_subj" %in% names(result$results))
  expect_true("power" %in% names(result$results))
  expect_true("convergence_rate" %in% names(result$results))
  expect_true(all(result$results$n_subj >= 10))
})
