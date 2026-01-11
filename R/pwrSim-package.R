#' pwrSim: Power Analysis for Mixed Effects Models via Simulation
#'
#' @description
#' The pwrSim package provides tools for estimating statistical power through
#' simulation for mixed effects models with interactions. It allows researchers
#' to determine adequate sample sizes and assess the power of their experimental
#' designs before data collection.
#'
#' @details
#' The main functions in this package are:
#' \itemize{
#'   \item \code{\link{simulate_power}}: Estimate power for a given design
#'   \item \code{\link{calculate_sample_size}}: Find required sample size for target power
#' }
#'
#' The package supports various random effect structures and interaction terms,
#' making it suitable for complex experimental designs common in psychology,
#' education, and other behavioral sciences.
#'
#' @section Key Features:
#' \itemize{
#'   \item Simulation-based power analysis for mixed effects models
#'   \item Support for interaction effects
#'   \item Flexible random effect structures
#'   \item Sample size determination
#'   \item Parallel processing for faster simulations
#' }
#'
#' @docType package
#' @name pwrSim-package
#' @aliases pwrSim
#'
#' @importFrom lme4 lmer
#' @importFrom stats rnorm formula pnorm
#' @importFrom parallel detectCores mclapply
#' @importFrom methods is
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
