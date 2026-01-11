#' Calculate Required Sample Size for Target Power
#'
#' This function helps determine the number of subjects needed to achieve
#' a target power level for a specific effect in a mixed effects model.
#'
#' @param target_power Numeric. Desired power level (e.g., 0.80 for 80% power).
#' @param effect_name Character. Name of the effect to power for.
#' @param n_obs_per_subj Integer. Number of observations per subject.
#' @param fixed_effects Named numeric vector. Fixed effect coefficients.
#' @param random_effects Named numeric vector. Random effect standard deviations.
#' @param residual_sd Numeric. Standard deviation of residual error. Default is 1.
#' @param n_subj_range Integer vector of length 2. Range of sample sizes to test.
#'   Default is c(10, 100).
#' @param n_sim Integer. Number of simulations per sample size. Default is 1000.
#' @param alpha Numeric. Significance level. Default is 0.05.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#' @param verbose Logical. Whether to print progress. Default is TRUE.
#'
#' @return A list with class "pwrSim_sample_size" containing:
#' \describe{
#'   \item{target_power}{Target power level}
#'   \item{recommended_n}{Recommended sample size}
#'   \item{results}{Data frame with power estimates for each sample size}
#'   \item{effect_name}{Name of the effect that was powered}
#' }
#'
#' @examples
#' \dontrun{
#' fixed_fx <- c(intercept = 5, effect1 = 0.5, effect2 = 0.3, interaction = 0.4)
#' random_fx <- c(subj_intercept = 1.0, subj_slope = 0.5)
#' 
#' ss_result <- calculate_sample_size(
#'   target_power = 0.80,
#'   effect_name = "interaction",
#'   n_obs_per_subj = 20,
#'   fixed_effects = fixed_fx,
#'   random_effects = random_fx,
#'   n_subj_range = c(20, 50),
#'   n_sim = 100,
#'   seed = 123
#' )
#' 
#' print(ss_result)
#' plot(ss_result)
#' }
#'
#' @export
calculate_sample_size <- function(target_power,
                                   effect_name,
                                   n_obs_per_subj,
                                   fixed_effects,
                                   random_effects,
                                   residual_sd = 1,
                                   n_subj_range = c(10, 100),
                                   n_sim = 1000,
                                   alpha = 0.05,
                                   seed = NULL,
                                   verbose = TRUE) {
  
  # Input validation
  if (target_power <= 0 || target_power >= 1) {
    stop("target_power must be between 0 and 1")
  }
  if (!effect_name %in% names(fixed_effects)) {
    stop(sprintf("effect_name '%s' not found in fixed_effects", effect_name))
  }
  if (length(n_subj_range) != 2 || n_subj_range[1] >= n_subj_range[2]) {
    stop("n_subj_range must be a vector of length 2 with increasing values")
  }
  
  # Generate sequence of sample sizes to test
  n_subj_seq <- seq(n_subj_range[1], n_subj_range[2], by = 5)
  
  if (verbose) {
    message(sprintf("Testing sample sizes from %d to %d subjects...",
                   n_subj_range[1], n_subj_range[2]))
  }
  
  # Run power analysis for each sample size
  results <- data.frame(
    n_subj = integer(),
    power = numeric(),
    convergence_rate = numeric()
  )
  
  for (n in n_subj_seq) {
    if (verbose) {
      message(sprintf("  Testing n = %d...", n))
    }
    
    power_result <- simulate_power(
      n_subj = n,
      n_obs_per_subj = n_obs_per_subj,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      residual_sd = residual_sd,
      n_sim = n_sim,
      alpha = alpha,
      seed = if (!is.null(seed)) seed + n else NULL,
      verbose = FALSE
    )
    
    # Get power for the effect (handle NaN/NA)
    effect_power <- power_result$power[effect_name]
    if (is.na(effect_power) || is.nan(effect_power)) {
      effect_power <- 0
    }
    
    results <- rbind(results, data.frame(
      n_subj = n,
      power = effect_power,
      convergence_rate = power_result$convergence_rate
    ))
    
    # Stop early if we've clearly exceeded target power
    if (!is.na(effect_power) && !is.nan(effect_power) && 
        effect_power > target_power + 0.05) {
      break
    }
  }
  
  # Find recommended sample size
  powered_results <- results[results$power >= target_power, ]
  
  if (nrow(powered_results) > 0) {
    recommended_n <- min(powered_results$n_subj)
    if (verbose) {
      message(sprintf("\nRecommended sample size: %d subjects (power = %.3f)",
                     recommended_n, 
                     powered_results$power[powered_results$n_subj == recommended_n]))
    }
  } else {
    recommended_n <- NA
    if (verbose) {
      message(sprintf("\nTarget power of %.2f not achieved in tested range.",
                     target_power))
      message(sprintf("Maximum power observed: %.3f at n = %d",
                     max(results$power), 
                     results$n_subj[which.max(results$power)]))
      message("Consider increasing n_subj_range or adjusting effect sizes.")
    }
  }
  
  # Create result object
  result <- list(
    target_power = target_power,
    recommended_n = recommended_n,
    results = results,
    effect_name = effect_name,
    parameters = list(
      n_obs_per_subj = n_obs_per_subj,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      residual_sd = residual_sd,
      alpha = alpha
    )
  )
  
  class(result) <- "pwrSim_sample_size"
  
  return(result)
}


#' Print Method for Sample Size Results
#'
#' @param x Object of class "pwrSim_sample_size"
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
print.pwrSim_sample_size <- function(x, ...) {
  cat("\n=== Sample Size Calculation Results ===\n\n")
  cat(sprintf("Target power: %.2f (%.0f%%)\n", x$target_power, x$target_power * 100))
  cat(sprintf("Effect: %s\n", x$effect_name))
  
  if (!is.na(x$recommended_n)) {
    actual_power <- x$results$power[x$results$n_subj == x$recommended_n]
    cat(sprintf("\nRecommended sample size: %d subjects\n", x$recommended_n))
    cat(sprintf("Achieved power: %.3f (%.1f%%)\n", actual_power, actual_power * 100))
  } else {
    cat("\nTarget power not achieved in tested range.\n")
    cat(sprintf("Maximum power: %.3f at n = %d subjects\n",
               max(x$results$power),
               x$results$n_subj[which.max(x$results$power)]))
  }
  
  cat("\nPower by sample size:\n")
  print(x$results, row.names = FALSE)
  
  invisible(x)
}


#' Plot Power Curve
#'
#' Creates a power curve plot showing the relationship between sample size
#' and statistical power.
#'
#' @param x Object of class "pwrSim_sample_size" from calculate_sample_size()
#' @param ... Additional arguments passed to plot
#'
#' @return NULL (creates a plot)
#' @export
plot.pwrSim_sample_size <- function(x, ...) {
  # Check if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(x$results, ggplot2::aes(x = n_subj, y = power)) +
      ggplot2::geom_line(color = "blue", size = 1) +
      ggplot2::geom_point(color = "blue", size = 2) +
      ggplot2::geom_hline(yintercept = x$target_power, 
                         linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = sprintf("Power Curve for %s", x$effect_name),
        x = "Number of Subjects",
        y = "Statistical Power",
        caption = sprintf("Target power: %.2f", x$target_power)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0.5)
      )
    
    if (!is.na(x$recommended_n)) {
      p <- p + ggplot2::geom_vline(xintercept = x$recommended_n,
                                   linetype = "dashed", color = "darkgreen")
    }
    
    print(p)
  } else {
    # Fallback to base graphics
    plot(x$results$n_subj, x$results$power,
         type = "b", pch = 19, col = "blue",
         xlab = "Number of Subjects",
         ylab = "Statistical Power",
         main = sprintf("Power Curve for %s", x$effect_name),
         ...)
    abline(h = x$target_power, lty = 2, col = "red")
    
    if (!is.na(x$recommended_n)) {
      abline(v = x$recommended_n, lty = 2, col = "darkgreen")
    }
    
    legend("bottomright",
           legend = c(sprintf("Target power: %.2f", x$target_power),
                     if (!is.na(x$recommended_n)) 
                       sprintf("Recommended n: %d", x$recommended_n) 
                     else NULL),
           lty = c(2, if (!is.na(x$recommended_n)) 2 else NULL),
           col = c("red", if (!is.na(x$recommended_n)) "darkgreen" else NULL),
           bty = "n")
  }
  
  invisible(NULL)
}
