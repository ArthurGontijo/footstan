#' Bayesian regression for football championships data
#'
#' @param data List with the championship data.
#' @param hyperparams List of hyper parameters for the desired model.
#' @param model Model of choice.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @export

fit_data <- function(data, hyperparams, model, iter=2000, chains=2, ...) {
  models <- names(stanmodels)

  if(!(model %in% models)){
    stop(paste0('Model "',  model, '" not found!'))
  }

  model_index <- which(models == model)
  model <- stanmodels[[model_index]]

  ngames <- nrow(data)
  nteams <- length(table(c(data$h, data$a)))
  meta_data <- list(ngames, nteams)
  standata <- c(data, hyperparams, meta_data)

  fit <- rstan::sampling(model, data = standata, iter = iter, chains = chains, ...)

  return(fit)
}
