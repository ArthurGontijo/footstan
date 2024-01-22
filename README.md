
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

First of all, we need to gather our data. Lets generate the results of all 380 matches of a 20 teams competitions, based on a poisson theoretical model. 

``` r
require(footstan)

poisson_params <- list(
  sd_att = 0.2,
  sd_def = 0.2,
  beta_0 = -0.05,
  home = 0.36  
)

games <- simulate_championship(num_teams = 20,
                               model = "poisson",
                               params = poisson_params)
```

Now that we have our data, we can fit a bayesian model provided by footstan to it. Suppose we want to fit a poisson model to our poisson generated data. First of all, we need to define the hyper parameters for our prior distributions. 

```r
poisson_hyperparams <- list(
  beta_0_mu = 0,
  beta_0_sd = 10,
  home_mu = 0,
  home_sd = 10,
  att_mu = 0,
  def_mu = 0,
  sd_att_mu = 0,
  sd_att_sig = 2.5,
  sd_def_mu = 0,
  sd_def_sig = 2.5
)
```

To fit the model, we simply run:

```r
fit <- fit_data(data = games, 
                hyperparams = poisson_hyperparams, 
                model = "poisson",
                chains = 2,
                iter = 2000)
```


