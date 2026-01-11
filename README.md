# pwrSim

<!-- badges: start -->
[![R-CMD-check](https://github.com/hdbt/pwrSim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hdbt/pwrSim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Power Analysis for Mixed Effects Models via Simulation

The `pwrSim` package provides tools for estimating statistical power through simulation for mixed effects models with interactions. It allows researchers to determine adequate sample sizes and assess the power of their experimental designs before data collection.

## Features

- **Simulation-based power analysis** for mixed effects models
- **Support for interaction effects** between predictors
- **Flexible random effect structures** (random intercepts and slopes)
- **Sample size determination** for target power levels
- **Parallel processing** for faster simulations
- **Comprehensive documentation** with examples and vignettes

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("hdbt/pwrSim")
```

## Quick Start

```r
library(pwrSim)

# Define effect sizes
fixed_effects <- c(
  intercept = 5.0,
  effect1 = 0.5,
  effect2 = 0.3,
  interaction = 0.4
)

# Define random effects (standard deviations)
random_effects <- c(
  subj_intercept = 1.0,
  subj_slope = 0.5
)

# Estimate power
result <- simulate_power(
  n_subj = 30,
  n_obs_per_subj = 20,
  fixed_effects = fixed_effects,
  random_effects = random_effects,
  residual_sd = 1.5,
  n_sim = 1000,
  seed = 123
)

print(result)
```

## Sample Size Calculation

```r
# Find required sample size for 80% power
ss_result <- calculate_sample_size(
  target_power = 0.80,
  effect_name = "interaction",
  n_obs_per_subj = 20,
  fixed_effects = fixed_effects,
  random_effects = random_effects,
  n_subj_range = c(20, 60),
  n_sim = 500
)

print(ss_result)
plot(ss_result)
```

## Documentation

- See the [Introduction vignette](vignettes/introduction.Rmd) for detailed examples
- Function documentation: `?simulate_power` and `?calculate_sample_size`

## Citation

If you use this package in your research, please cite:

```
To cite pwrSim in publications use:

  Bulut (2025). pwrSim: Power Analysis for Mixed Effects Models
  via Simulation. R package version 0.1.0.
  https://github.com/hdbt/pwrSim

A BibTeX entry for LaTeX users is:

@Manual{Bulut2025pwrSim,
  title  = {pwrSim: Power Analysis for Mixed Effects Models via Simulation},
  author = {Hamid Bulut},
  year   = {2025},
  note   = {R package version 0.1.0},
  url    = {https://github.com/hdbt/pwrSim}
}

```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
