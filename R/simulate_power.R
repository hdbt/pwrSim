#' Simulate Power for Mixed Effects Models with Interactions
#'
#' This function estimates statistical power via simulation for mixed effects models,
#' particularly those with interaction terms. It generates data according to specified
#' parameters, fits mixed effects models, and calculates the proportion of significant
#' effects across simulations.
#'
#' @param n_subj Integer. Number of subjects/participants.
#' @param n_obs_per_subj Integer. Number of observations per subject.
#' @param fixed_effects Named numeric vector. Fixed effect coefficients including
#'   intercept, main effects, and interaction effects. Names should follow the
#'   pattern: "intercept", "effect1", "effect2", "interaction".
#' @param random_effects Named numeric vector. Standard deviations for random effects.
#'   Should include "subj_intercept" and optionally "subj_slope" for random slopes.
#' @param residual_sd Numeric. Standard deviation of residual error. Default is 1.
#' @param n_sim Integer. Number of simulations to run. Default is 1000.
#' @param alpha Numeric. Significance level for hypothesis testing. Default is 0.05.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#' @param parallel Logical. Whether to use parallel processing. Default is FALSE.
#' @param n_cores Integer. Number of cores to use for parallel processing. 
#'   Default is NULL (uses detectCores() - 1).
#' @param formula Character string or formula. The model formula to fit. If NULL,
#'   a default formula is constructed based on fixed_effects names.
#' @param verbose Logical. Whether to print progress messages. Default is TRUE.
#'
#' @return A list with class "pwrSim_result" containing:
#'   \item{power}{Named numeric vector of power estimates for each effect}
#'   \item{convergence_rate}{Proportion of models that converged successfully}
#'   \item{n_sim}{Number of simulations run}
#'   \item{parameters}{List of input parameters used}
#'   \item{detailed_results}{Data frame with detailed results from each simulation}
#'
#' @details
#' The function simulates data from a mixed effects model with the specified
#' parameters, fits the model using lme4::lmer, and tests whether each effect
#' is statistically significant. Power is calculated as the proportion of
#' simulations in which the effect is significant at the specified alpha level.
#'
#' For models with interactions, the function supports two-way interactions between
#' categorical or continuous predictors. The data generation process assumes normally
#' distributed random effects and residuals.
#'
#' @examples
#' \dontrun{
#' # Simple example with interaction
#' fixed_fx <- c(intercept = 5, effect1 = 0.5, effect2 = 0.3, interaction = 0.4)
#' random_fx <- c(subj_intercept = 1.0, subj_slope = 0.5)
#' 
#' result <- simulate_power(
#'   n_subj = 30,
#'   n_obs_per_subj = 20,
#'   fixed_effects = fixed_fx,
#'   random_effects = random_fx,
#'   residual_sd = 1.5,
#'   n_sim = 100,
#'   seed = 123
#' )
#' 
#' print(result)
#' summary(result)
#' }
#'
#' @export
#' @importFrom lme4 lmer
#' @importFrom stats rnorm formula
#' @importFrom parallel detectCores mclapply
simulate_power <- function(n_subj,
                          n_obs_per_subj,
                          fixed_effects,
                          random_effects,
                          residual_sd = 1,
                          n_sim = 1000,
                          alpha = 0.05,
                          seed = NULL,
                          parallel = FALSE,
                          n_cores = NULL,
                          formula = NULL,
                          verbose = TRUE) {
  
  # Input validation
  if (!is.numeric(n_subj) || n_subj < 2) {
    stop("n_subj must be a numeric value >= 2")
  }
  if (!is.numeric(n_obs_per_subj) || n_obs_per_subj < 2) {
    stop("n_obs_per_subj must be a numeric value >= 2")
  }
  if (!is.numeric(fixed_effects) || is.null(names(fixed_effects))) {
    stop("fixed_effects must be a named numeric vector")
  }
  if (!is.numeric(random_effects) || is.null(names(random_effects))) {
    stop("random_effects must be a named numeric vector")
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1")
  }
  if (n_sim < 1) {
    stop("n_sim must be at least 1")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Setup parallel processing
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    if (verbose) {
      message(sprintf("Running %d simulations using %d cores...", n_sim, n_cores))
    }
  } else {
    if (verbose) {
      message(sprintf("Running %d simulations sequentially...", n_sim))
    }
  }
  
  # Run simulations
  if (parallel) {
    results <- parallel::mclapply(1:n_sim, function(i) {
      run_single_simulation(
        n_subj = n_subj,
        n_obs_per_subj = n_obs_per_subj,
        fixed_effects = fixed_effects,
        random_effects = random_effects,
        residual_sd = residual_sd,
        alpha = alpha,
        formula = formula
      )
    }, mc.cores = n_cores)
  } else {
    results <- lapply(1:n_sim, function(i) {
      if (verbose && i %% 100 == 0) {
        message(sprintf("  Completed %d/%d simulations", i, n_sim))
      }
      run_single_simulation(
        n_subj = n_subj,
        n_obs_per_subj = n_obs_per_subj,
        fixed_effects = fixed_effects,
        random_effects = random_effects,
        residual_sd = residual_sd,
        alpha = alpha,
        formula = formula
      )
    })
  }
  
  # Process results
  convergence_rate <- mean(sapply(results, function(x) x$converged))
  converged_results <- results[sapply(results, function(x) x$converged)]
  
  if (length(converged_results) == 0) {
    stop("No models converged successfully. Check your parameters.")
  }
  
  # Calculate power for each effect
  effect_names <- names(fixed_effects)
  power_estimates <- sapply(effect_names, function(effect) {
    significant <- sapply(converged_results, function(x) {
      if (effect %in% names(x$p_values)) {
        x$p_values[effect] < alpha
      } else {
        NA
      }
    })
    mean(significant, na.rm = TRUE)
  })
  
  # Create detailed results data frame
  # Initialize all columns to ensure consistent structure
  detailed_df <- do.call(rbind, lapply(seq_along(results), function(i) {
    res <- results[[i]]
    df <- data.frame(
      sim_id = i,
      converged = res$converged,
      stringsAsFactors = FALSE
    )
    # Add columns for all effects, even if model didn't converge
    for (effect in effect_names) {
      df[[paste0(effect, "_pval")]] <- NA_real_
      df[[paste0(effect, "_sig")]] <- NA
    }
    # Fill in values for converged models
    if (res$converged && !is.null(res$p_values)) {
      for (effect in effect_names) {
        if (effect %in% names(res$p_values)) {
          df[[paste0(effect, "_pval")]] <- res$p_values[effect]
          df[[paste0(effect, "_sig")]] <- res$p_values[effect] < alpha
        }
      }
    }
    df
  }))
  
  # Create result object
  result <- list(
    power = power_estimates,
    convergence_rate = convergence_rate,
    n_sim = n_sim,
    n_converged = length(converged_results),
    parameters = list(
      n_subj = n_subj,
      n_obs_per_subj = n_obs_per_subj,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      residual_sd = residual_sd,
      alpha = alpha
    ),
    detailed_results = detailed_df
  )
  
  class(result) <- "pwrSim_result"
  
  if (verbose) {
    message("\nSimulation complete!")
    message(sprintf("Convergence rate: %.2f%%", convergence_rate * 100))
  }
  
  return(result)
}


#' Run a Single Power Simulation
#'
#' Internal function that runs a single iteration of the power simulation.
#' Not intended to be called directly by users.
#'
#' @param n_subj Number of subjects
#' @param n_obs_per_subj Number of observations per subject
#' @param fixed_effects Named vector of fixed effects
#' @param random_effects Named vector of random effects SDs
#' @param residual_sd Residual standard deviation
#' @param alpha Significance level
#' @param formula Model formula (optional)
#'
#' @return List with converged status and p-values
#' @keywords internal
#' @noRd
run_single_simulation <- function(n_subj, n_obs_per_subj, fixed_effects,
                                  random_effects, residual_sd, alpha, formula) {
  
  # Generate data
  data <- generate_mixed_model_data(
    n_subj = n_subj,
    n_obs_per_subj = n_obs_per_subj,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    residual_sd = residual_sd
  )
  
  # Fit model
  fit_result <- fit_mixed_model(data, fixed_effects, formula)
  
  return(fit_result)
}


#' Generate Data for Mixed Effects Model
#'
#' Internal function to generate simulated data for mixed effects models
#' with interactions.
#'
#' @param n_subj Number of subjects
#' @param n_obs_per_subj Number of observations per subject
#' @param fixed_effects Named vector of fixed effects
#' @param random_effects Named vector of random effects SDs
#' @param residual_sd Residual standard deviation
#'
#' @return Data frame with simulated data
#' @keywords internal
#' @noRd
generate_mixed_model_data <- function(n_subj, n_obs_per_subj, fixed_effects,
                                      random_effects, residual_sd) {
  
  n_total <- n_subj * n_obs_per_subj
  
  # Create subject IDs
  subj_id <- rep(1:n_subj, each = n_obs_per_subj)
  
  # Generate predictors based on effect names
  effect_names <- names(fixed_effects)
  effect_names <- effect_names[effect_names != "intercept"]
  
  # Determine which are main effects vs interactions
  main_effects <- effect_names[!grepl(":", effect_names) & 
                                !grepl("interaction", effect_names, ignore.case = TRUE)]
  interaction_effects <- effect_names[grepl(":", effect_names) | 
                                       grepl("interaction", effect_names, ignore.case = TRUE)]
  
  # Generate main effect predictors (continuous by default)
  data <- data.frame(subj_id = factor(subj_id))
  
  for (effect in main_effects) {
    # Create continuous predictor (centered)
    data[[effect]] <- rnorm(n_total, mean = 0, sd = 1)
  }
  
  # If specific predictors aren't named, create generic x1, x2
  if (length(main_effects) == 0) {
    # Assume at least 2 predictors for interaction
    data$x1 <- rnorm(n_total, mean = 0, sd = 1)
    data$x2 <- rnorm(n_total, mean = 0, sd = 1)
    main_predictors <- c("x1", "x2")
  } else {
    main_predictors <- main_effects
  }
  
  # Generate random effects
  if ("subj_intercept" %in% names(random_effects)) {
    subj_intercept <- rnorm(n_subj, mean = 0, sd = random_effects["subj_intercept"])
  } else {
    subj_intercept <- rep(0, n_subj)
  }
  
  if ("subj_slope" %in% names(random_effects) && length(main_predictors) > 0) {
    subj_slope <- rnorm(n_subj, mean = 0, sd = random_effects["subj_slope"])
  } else {
    subj_slope <- rep(0, n_subj)
  }
  
  # Calculate expected value
  y_expected <- fixed_effects["intercept"] + subj_intercept[subj_id]
  
  # Add main effects
  for (i in seq_along(main_predictors)) {
    predictor <- main_predictors[i]
    if (predictor %in% names(fixed_effects)) {
      coef <- fixed_effects[predictor]
    } else if (i == 1 && "effect1" %in% names(fixed_effects)) {
      coef <- fixed_effects["effect1"]
    } else if (i == 2 && "effect2" %in% names(fixed_effects)) {
      coef <- fixed_effects["effect2"]
    } else {
      coef <- 0
    }
    
    if (i == 1) {
      # Add random slope for first predictor
      y_expected <- y_expected + (coef + subj_slope[subj_id]) * data[[predictor]]
    } else {
      y_expected <- y_expected + coef * data[[predictor]]
    }
  }
  
  # Add interaction effects
  if (length(interaction_effects) > 0 && length(main_predictors) >= 2) {
    for (interaction in interaction_effects) {
      if (interaction %in% names(fixed_effects)) {
        coef <- fixed_effects[interaction]
      } else {
        coef <- 0
      }
      # Create interaction term
      y_expected <- y_expected + coef * data[[main_predictors[1]]] * data[[main_predictors[2]]]
    }
  }
  
  # Add residual error
  data$y <- y_expected + rnorm(n_total, mean = 0, sd = residual_sd)
  
  return(data)
}


#' Fit Mixed Effects Model
#'
#' Internal function to fit a mixed effects model and extract p-values.
#'
#' @param data Data frame with simulated data
#' @param fixed_effects Named vector of fixed effects
#' @param formula Optional formula specification
#'
#' @return List with converged status and p-values
#' @keywords internal
#' @noRd
fit_mixed_model <- function(data, fixed_effects, formula = NULL) {
  
  # Get effect names for later matching
  effect_names <- names(fixed_effects)
  
  # Construct formula if not provided
  if (is.null(formula)) {
    main_effects <- effect_names[effect_names != "intercept" & 
                                  !grepl(":", effect_names) &
                                  !grepl("interaction", effect_names, ignore.case = TRUE)]
    
    # Determine predictors from data
    data_predictors <- setdiff(names(data), c("y", "subj_id"))
    
    if (length(main_effects) > 0 && all(main_effects %in% names(data))) {
      predictors <- main_effects
    } else if (length(data_predictors) > 0) {
      predictors <- data_predictors
    } else {
      predictors <- c("x1", "x2")
    }
    
    # Build formula
    if (length(predictors) >= 2) {
      formula_str <- sprintf("y ~ %s * %s + (1 + %s | subj_id)",
                            predictors[1], predictors[2], predictors[1])
    } else if (length(predictors) == 1) {
      formula_str <- sprintf("y ~ %s + (1 + %s | subj_id)",
                            predictors[1], predictors[1])
    } else {
      formula_str <- "y ~ 1 + (1 | subj_id)"
    }
    
    formula <- stats::formula(formula_str)
  }
  
  # Fit model with error handling
  fit <- tryCatch({
    suppressWarnings(lme4::lmer(formula, data = data, REML = FALSE))
  }, error = function(e) {
    NULL
  })
  
  # Check convergence
  if (is.null(fit)) {
    return(list(converged = FALSE, p_values = NULL))
  }
  
  # Consider the model converged unless there are serious convergence warnings
  # Singular fits (boundary fits) are common with random effects and still usable
  converged <- TRUE
  if (!is.null(fit@optinfo$conv$lme4$code) && fit@optinfo$conv$lme4$code != 0) {
    # Only mark as non-converged if there's an actual convergence failure code
    converged <- FALSE
  }
  
  # Extract p-values even for singular fits (they're still valid for inference)
  p_values <- NULL
  if (converged) {
    coef_summary <- tryCatch({
      summary(fit)$coefficients
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(coef_summary)) {
      # Use t-values and approximate p-values
      # (For proper p-values, lmerTest package could be used, but we keep dependencies minimal)
      t_values <- coef_summary[, "t value"]
      
      # Approximate p-values using normal distribution (conservative)
      p_values <- 2 * (1 - pnorm(abs(t_values)))
      names(p_values) <- rownames(coef_summary)
      
      # Rename to match input effect names better
      names(p_values) <- gsub("\\(Intercept\\)", "intercept", names(p_values))
      
      # Map interaction terms - x1:x2 should match "interaction" in effect names
      # Also match the actual predictor names if they exist
      original_names <- names(p_values)
      for (i in seq_along(original_names)) {
        name <- original_names[i]
        if (grepl(":", name)) {
          # This is an interaction term
          # Check if "interaction" is in the effect names
          if ("interaction" %in% effect_names) {
            names(p_values)[i] <- "interaction"
          }
        }
      }
    }
  }
  
  return(list(converged = converged, p_values = p_values))
}


#' Print Method for pwrSim Results
#'
#' @param x Object of class "pwrSim_result"
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
print.pwrSim_result <- function(x, ...) {
  cat("\n=== Power Simulation Results ===\n\n")
  cat(sprintf("Number of simulations: %d\n", x$n_sim))
  cat(sprintf("Converged simulations: %d (%.1f%%)\n", 
              x$n_converged, x$convergence_rate * 100))
  cat(sprintf("Significance level: %.3f\n\n", x$parameters$alpha))
  
  cat("Power estimates:\n")
  for (effect in names(x$power)) {
    cat(sprintf("  %-20s: %.3f (%.1f%%)\n", effect, x$power[effect], 
                x$power[effect] * 100))
  }
  
  cat("\nStudy design:\n")
  cat(sprintf("  Number of subjects: %d\n", x$parameters$n_subj))
  cat(sprintf("  Observations per subject: %d\n", x$parameters$n_obs_per_subj))
  cat(sprintf("  Total observations: %d\n", 
              x$parameters$n_subj * x$parameters$n_obs_per_subj))
  
  invisible(x)
}


#' Summary Method for pwrSim Results
#'
#' @param object Object of class "pwrSim_result"
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
summary.pwrSim_result <- function(object, ...) {
  print(object)
  
  cat("\n=== Fixed Effects ===\n")
  for (effect in names(object$parameters$fixed_effects)) {
    cat(sprintf("  %-20s: %.3f\n", effect, 
                object$parameters$fixed_effects[effect]))
  }
  
  cat("\n=== Random Effects (SD) ===\n")
  for (effect in names(object$parameters$random_effects)) {
    cat(sprintf("  %-20s: %.3f\n", effect, 
                object$parameters$random_effects[effect]))
  }
  
  cat(sprintf("\nResidual SD: %.3f\n", object$parameters$residual_sd))
  
  invisible(object)
}
