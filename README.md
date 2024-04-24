
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

# beta_0 ~ N(0, 10)
beta_0_hyp <- c(0, 10)

# home ~ N(0, 10)
home_hyp <- c(0, 10)

# sd_att ~ Cauchy(0, 2.5)
sd_att_hyp <- c(0, 2.5)

# sd_def ~ Cauchy(0, 2.5)
sd_def_hyp <- c(0, 2.5)
```

To fit the model, we simply run:

```r
fit <- fit_data(data = games, 
                beta_0_hyp = beta_0_hyp,
                home_hyp = home_hyp,
                sd_att_hyp = sd_att_hyp,
                sd_def_hyp = sd_def_hyp,
                chains = 2,
                iter = 2000)
```

Now you can diagnose how well Stan sampled the parameters:

```r
visual_diagnostic(fit, "att[1]")
visual_diagnostic(fit, "beta_0")
```

And you can even use new data to make predictions:

```r
new_data <- data.frame(h = c(1, 3), a = c(2, 4))
predictions <- predict(fit, new_data)
plot_predictions(predictions)
```


