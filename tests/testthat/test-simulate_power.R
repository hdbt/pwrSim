test_that("simulate_power works with basic input", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5, effect2 = 0.3, interaction = 0.4)
  random_fx <- c(subj_intercept = 1.0, subj_slope = 0.5)
  
  result <- simulate_power(
    n_subj = 20,
    n_obs_per_subj = 10,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    residual_sd = 1.5,
    n_sim = 10,  # Small number for testing
    seed = 123,
    verbose = FALSE
  )
  
  expect_s3_class(result, "pwrSim_result")
  expect_true("power" %in% names(result))
  expect_true("convergence_rate" %in% names(result))
  expect_true("parameters" %in% names(result))
  expect_equal(result$n_sim, 10)
})


test_that("simulate_power validates input correctly", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5)
  random_fx <- c(subj_intercept = 1.0)
  
  # Test invalid n_subj
  expect_error(
    simulate_power(n_subj = 1, n_obs_per_subj = 10, 
                  fixed_effects = fixed_fx, random_effects = random_fx),
    "n_subj must be a numeric value >= 2"
  )
  
  # Test invalid n_obs_per_subj
  expect_error(
    simulate_power(n_subj = 10, n_obs_per_subj = 1,
                  fixed_effects = fixed_fx, random_effects = random_fx),
    "n_obs_per_subj must be a numeric value >= 2"
  )
  
  # Test invalid alpha
  expect_error(
    simulate_power(n_subj = 10, n_obs_per_subj = 10,
                  fixed_effects = fixed_fx, random_effects = random_fx,
                  alpha = 1.5),
    "alpha must be between 0 and 1"
  )
  
  # Test unnamed fixed_effects
  expect_error(
    simulate_power(n_subj = 10, n_obs_per_subj = 10,
                  fixed_effects = c(5, 0.5), random_effects = random_fx),
    "fixed_effects must be a named numeric vector"
  )
})


test_that("power estimates are reasonable", {
  # Test with large effect size - should have high power
  # Simple model without random slopes to avoid convergence issues in tests
  fixed_fx <- c(intercept = 5, effect1 = 2.0)  # Large effect
  random_fx <- c(subj_intercept = 1.0)  # Only random intercept
  
  result <- simulate_power(
    n_subj = 30,
    n_obs_per_subj = 20,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    residual_sd = 1.5,
    n_sim = 50,
    seed = 456,
    verbose = FALSE
  )
  
  # With large effect, should have decent power
  expect_true(result$power["effect1"] > 0.5)
  # Should have some converged models
  expect_true(result$n_converged >= 10)
})


test_that("print method works", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5)
  random_fx <- c(subj_intercept = 1.0)
  
  result <- simulate_power(
    n_subj = 15,
    n_obs_per_subj = 10,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_sim = 5,
    seed = 789,
    verbose = FALSE
  )
  
  expect_output(print(result), "Power Simulation Results")
  expect_output(print(result), "Number of simulations")
})


test_that("summary method works", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5)
  random_fx <- c(subj_intercept = 1.0)
  
  result <- simulate_power(
    n_subj = 15,
    n_obs_per_subj = 10,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_sim = 5,
    seed = 101,
    verbose = FALSE
  )
  
  expect_output(summary(result), "Fixed Effects")
  expect_output(summary(result), "Random Effects")
})


test_that("detailed results data frame is created correctly", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5)
  random_fx <- c(subj_intercept = 1.0)
  
  result <- simulate_power(
    n_subj = 15,
    n_obs_per_subj = 10,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_sim = 10,
    seed = 202,
    verbose = FALSE
  )
  
  expect_true(is.data.frame(result$detailed_results))
  expect_equal(nrow(result$detailed_results), 10)
  expect_true("sim_id" %in% names(result$detailed_results))
  expect_true("converged" %in% names(result$detailed_results))
})


test_that("seed produces reproducible results", {
  fixed_fx <- c(intercept = 5, effect1 = 0.5)
  random_fx <- c(subj_intercept = 1.0)
  
  result1 <- simulate_power(
    n_subj = 15,
    n_obs_per_subj = 10,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_sim = 20,
    seed = 999,
    verbose = FALSE
  )
  
  result2 <- simulate_power(
    n_subj = 15,
    n_obs_per_subj = 10,
    fixed_effects = fixed_fx,
    random_effects = random_fx,
    n_sim = 20,
    seed = 999,
    verbose = FALSE
  )
  
  expect_equal(result1$power, result2$power)
  expect_equal(result1$convergence_rate, result2$convergence_rate)
})
