#' Bayesian regression for football championships data using the poisson model
#'
#' @param data Footbal games data
#' @param beta_0_hyp Vector of hyperparameters for beta_0 ~ N(beta_0_hyp[1], beta_0_hyp[2])
#' @param home_hyp Vector of hyperparameters for home ~ N(home_hyp[1], home_hyp[2])
#' @param att_sd_hyp Vector of hyperparameters for att_sd ~ Cauchy(att_sd_hyp[1], att_sd_hyp[2])
#' @param def_sd_hyp Vector of hyperparameters for def_sd ~ Cauchy(def_sd_hyp[1], def_sd_hyp[2])
#' @param iter Number of iterations used by `rstan::sampling`
#' @param chain Number of chains used by `rstan::sampling`
#' @param ... Other arguments passed to `rstan::sampling`
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @export

fit_poisson <- function(data, beta_0_hyp, home_hyp, att_sd_hyp, def_sd_hyp, iter, chains, ...){
  model <- stanmodels[["poisson"]]
  ngames <- nrow(data)
  nteams <- length(table(c(data$h, data$a)))
  meta_data <- list(ngames, nteams)

  hyperparameters <- list(
    beta_0_mu = beta_0_hyp[1],
    beta_0_sd = beta_0_hyp[2],
    home_mu = home_hyp[1],
    home_sd = home_hyp[2],
    att_mu = 0,
    def_mu = 0,
    sd_att_mu = att_sd_hyp[1],
    sd_att_sig = att_sd_hyp[2],
    sd_def_mu = def_sd_hyp[1],
    sd_def_sig = def_sd_hyp[2]
  )

  standata <- c(data, hyperparameters, meta_data)

  fit <- rstan::sampling(model, data = standata, iter = iter, chains = chains, ...)

  return(fit)
}

#' Bayesian regression for football championships data using the negative binomial model
#'
#' @param data Footbal games data
#' @param beta_0_hyp Vector of hyperparameters for beta_0 ~ N(beta_0_hyp[1], beta_0_hyp[2])
#' @param home_hyp Vector of hyperparameters for home ~ N(home_hyp[1], home_hyp[2])
#' @param att_sd_hyp Vector of hyperparameters for att_sd ~ Cauchy(att_sd_hyp[1], att_sd_hyp[2])
#' @param def_sd_hyp Vector of hyperparameters for def_sd ~ Cauchy(def_sd_hyp[1], def_sd_hyp[2])
#' @param phi_home_hyp Vector of hyperparameters for phy_home ~ Cauchy(phi_home_hyp[1], phi_home_hyp[2])
#' @param phi_away_hyp Vector of hyperparameters for phi_away ~ Cauchy(phi_away_hyp[1], phi_away_hyp[2])
#' @param iter Number of iterations used by `rstan::sampling`
#' @param chain Number of chains used by `rstan::sampling`
#' @param ... Other arguments passed to `rstan::sampling`
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @export

fit_negbinom <- function(data, beta_0_hyp, home_hyp, att_sd_hyp, def_sd_hyp, phi_home_hyp, phi_away_hyp, iter, chains, ...){
  model <- stanmodels[["negbinom"]]
  ngames <- nrow(data)
  nteams <- length(table(c(data$h, data$a)))
  meta_data <- list(ngames, nteams)

  hyperparameters <- list(
    beta_0_mu = beta_0_hyp[1],
    beta_0_sd = beta_0_hyp[2],
    home_mu = home_hyp[1],
    home_sd = home_hyp[2],
    att_mu = 0,
    def_mu = 0,
    sd_att_mu = att_sd_hyp[1],
    sd_att_sig = att_sd_hyp[2],
    sd_def_mu = def_sd_hyp[1],
    sd_def_sig = def_sd_hyp[2],
    phi_home_mu = phi_home_hyp[1],
    phi_home_sig = phi_home_hyp[2],
    phi_away_mu = phi_away_hyp[1],
    phi_away_sig = phi_away_hyp[2]
  )

  standata <- c(data, hyperparameters, meta_data)

  fit <- rstan::sampling(model, data = standata, iter = iter, chains = chains, ...)

  return(fit)
}





