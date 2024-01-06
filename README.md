
# footstan

<!-- badges: start -->
<!-- badges: end -->

The goal of footstan is to be a tool for adjusting bayesian models to football results.

## Installation

You can install the development version of footstan from [GitHub](https://github.com/) with:

``` r
if(!require(remotes)) install.packages("remotes")
devtools::install_github("ArthurGontijo/footstan")
```

## Example

Generating a sample of 200 championships of 20 teams based on a simple poisson model:

``` r
require(footstan)

poisson_params <- list(
  sd_att = 0.2,
  sd_def = 0.2,
  beta_0 = -0.05,
  home = 0.36  
)

generate_sample_data(simulations = 200, 
                     num_teams = 20, 
                     model = "poisson", 
                     params = poisson_params)
```

