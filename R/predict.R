#' Predicting new data using an already fitted model
#'
#' @param fit A footstanfit object.
#' @param new_data List with the championship data.
#' @return A vector with generated samples from the fitted model
#' @export

predict <- function(fit, new_data){
  model <- fit@model_name

  fit_data <- rstan::extract(fit)
  beta_0 <- fit_data$beta_0
  home <- fit_data$home
  att <- fit_data$att
  def <- fit_data$def
  n_samples <- nrow(beta_0)

  if(model == "poisson"){
    y1 <- list()
    y2 <- list()
    for(i in 1:nrow(new_data)){
      h <- new_data[i, ]$h
      a <- new_data[i, ]$a

      theta_1 <- beta_0 + home + att[, h] + def[, a]
      theta_2 <- beta_0 + att[, a] + def[, h]

      y1_new <- rpois(n_samples, exp(theta_1))
      y2_new <- rpois(n_samples, exp(theta_2))

      y1[[paste0("y1_", i)]] <- y1_new
      y2[[paste0("y2_", i)]] <- y2_new
    }
  }

  return(c(y1, y2, new_data))
}

#' Predicting new data using an already fitted model
#'
#' @param prediction_data An object with the predictions made by the function footstan::predict
#' @return A data frame with the proportions of wins, draws and loses for the home team in the new_data
#' @export

prediction_proportions <- function(prediction_data){
  predicted_games <- data.frame(h = prediction_data$h, a = prediction_data$a,
                                home_win = NA, draw = NA, home_lost = NA)

  total_games <- nrow(predicted_games)
  for(i in 1:total_games){
    y1_pred <- prediction_data[[paste0("y1_", i)]]
    y2_pred <- prediction_data[[paste0("y2_", i)]]
    n_preds <- length(y1_pred)

    predicted_games[i, ]$home_win <- sum(y1_pred > y2_pred)/n_preds
    predicted_games[i, ]$draw <- sum(y1_pred == y2_pred)/n_preds
    predicted_games[i, ]$home_lost <- sum(y1_pred < y2_pred)/n_preds
  }

  return(predicted_games)
}






