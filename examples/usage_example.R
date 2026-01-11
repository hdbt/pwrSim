#!/usr/bin/env Rscript
# Example usage of the pwrSim package
# This script demonstrates how to use the package for power analysis

# Load the package
# In practice, you would use: library(pwrSim)
# For development, we use devtools::load_all()
if (require("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(pwrSim)
}

# =============================================================================
# Example 1: Basic Power Analysis with Interaction
# =============================================================================

cat("\n=== Example 1: Basic Power Analysis ===\n\n")

# Define effect sizes for a study with two main effects and their interaction
fixed_effects <- c(
  intercept = 5.0,      # Overall mean response
  effect1 = 0.5,        # Main effect of predictor 1
  effect2 = 0.3,        # Main effect of predictor 2
  interaction = 0.4     # Interaction between predictor 1 and 2
)

# Define random effects (standard deviations)
random_effects <- c(
  subj_intercept = 1.0,  # Between-subject variability in baseline
  subj_slope = 0.5       # Between-subject variability in response to predictor 1
)

# Run power analysis
cat("Running power simulation with 100 iterations...\n")
result1 <- simulate_power(
  n_subj = 30,              # 30 subjects
  n_obs_per_subj = 20,      # 20 observations per subject
  fixed_effects = fixed_effects,
  random_effects = random_effects,
  residual_sd = 1.5,        # Within-subject variability
  n_sim = 100,              # Number of simulations (use 1000+ in practice)
  seed = 123,               # For reproducibility
  verbose = TRUE
)

# Display results
print(result1)
cat("\n")
summary(result1)

# =============================================================================
# Example 2: Sample Size Determination
# =============================================================================

cat("\n\n=== Example 2: Sample Size Determination ===\n\n")

# Find the required sample size to achieve 80% power for the interaction effect
cat("Determining sample size needed for 80% power on interaction...\n")
ss_result <- calculate_sample_size(
  target_power = 0.80,
  effect_name = "interaction",
  n_obs_per_subj = 20,
  fixed_effects = fixed_effects,
  random_effects = random_effects,
  residual_sd = 1.5,
  n_subj_range = c(20, 50),  # Test sample sizes from 20 to 50
  n_sim = 50,                 # Use 500+ in practice
  seed = 456,
  verbose = TRUE
)

# Display results
print(ss_result)

# Create power curve plot (if ggplot2 is available)
cat("\nAttempting to create power curve plot...\n")
tryCatch({
  plot(ss_result)
}, error = function(e) {
  cat("Note: Install ggplot2 for prettier plots, or a basic plot was created.\n")
})

# =============================================================================
# Example 3: Simpler Model (Only Main Effect)
# =============================================================================

cat("\n\n=== Example 3: Simple Main Effect Analysis ===\n\n")

# Simple model with just one main effect
simple_fixed <- c(
  intercept = 10,
  effect1 = 1.0    # Larger effect size
)

simple_random <- c(
  subj_intercept = 2.0
)

cat("Running power simulation for simple main effect...\n")
result3 <- simulate_power(
  n_subj = 25,
  n_obs_per_subj = 15,
  fixed_effects = simple_fixed,
  random_effects = simple_random,
  residual_sd = 2.0,
  n_sim = 50,
  seed = 789,
  verbose = TRUE
)

print(result3)

# =============================================================================
# Example 4: Parallel Processing (for larger simulations)
# =============================================================================

cat("\n\n=== Example 4: Parallel Processing ===\n\n")

cat("Note: This example shows how to use parallel processing for large simulations.\n")
cat("For demonstration, we'll use a small number of simulations.\n\n")

# Uncomment the following to run a larger simulation with parallel processing:
# result4 <- simulate_power(
#   n_subj = 40,
#   n_obs_per_subj = 20,
#   fixed_effects = fixed_effects,
#   random_effects = random_effects,
#   residual_sd = 1.5,
#   n_sim = 5000,           # Large number of simulations
#   parallel = TRUE,         # Enable parallel processing
#   n_cores = 4,            # Use 4 cores
#   seed = 999
# )

cat("To enable parallel processing, set parallel = TRUE and specify n_cores.\n")
cat("Example:\n")
cat("  result <- simulate_power(..., n_sim = 5000, parallel = TRUE, n_cores = 4)\n")

# =============================================================================
# Summary and Tips
# =============================================================================

cat("\n\n=== Tips for Using pwrSim ===\n\n")
cat("1. Use realistic effect sizes based on pilot data or literature\n")
cat("2. Run at least 1000 simulations for stable power estimates\n")
cat("3. Check convergence rates - low rates suggest model misspecification\n")
cat("4. Consider multiple scenarios (optimistic, realistic, pessimistic)\n")
cat("5. Plan for participant dropout by increasing sample size by 10-20%\n")
cat("6. Use parallel processing for large simulations (n_sim > 1000)\n")
cat("\nFor more information, see: ?simulate_power and ?calculate_sample_size\n")
cat("Or view the vignette: vignette('introduction', package = 'pwrSim')\n\n")
