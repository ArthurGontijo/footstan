
# footstan

<!-- badges: start -->
<!-- badges: end -->

The goal of footstan is to be a tool for adjusting bayesian models to football results.

## Installation

### On Linux:

``` r
install.packages("https://github.com/ArthurGontijo/footstan/releases/download/v1.0.0/footstan.tar.gz", repos = NULL, type = "source")
```

### On Windows:

``` r
install.packages("https://github.com/ArthurGontijo/footstan/releases/download/v1.0.0/footstan.zip", repos = NULL, type = "win.binary")
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

# att_sd ~ Cauchy(0, 2.5)
att_sd_hyp <- c(0, 2.5)

# def_sd ~ Cauchy(0, 2.5)
def_sd_hyp <- c(0, 2.5)
```

To fit the model, we simply run:

```r
fit <- fit_poisson(data = games, 
                beta_0_hyp = beta_0_hyp,
                home_hyp = home_hyp,
                att_sd_hyp = att_sd_hyp,
                def_sd_hyp = def_sd_hyp,
                chains = 2,
                iter = 2000)
```

Now you can diagnose how well Stan sampled the parameters:

```r
visual_diagnostic(fit, c("beta_0", "home", "att[1]"))
```

And you can even use new data to make predictions:

```r
new_data <- data.frame(h = c(1, 3), a = c(2, 4))
predictions <- predict(fit, new_data)
plot_predictions(predictions)
```
