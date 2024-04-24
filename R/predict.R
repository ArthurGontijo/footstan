#' Predicting new data using an already fitted model
#'
#' @param fit A footstanfit object.
#' @param new_data List with the championship data.
#' @return A vector with generated samples from the fitted model
#' @export
#'

predict <- function(fit, new_data){
  model <- fit@model_name
  special_models <- c("poisson_2", "negbinom_2", "poisson_infl_2")

  fit_data <- rstan::extract(fit)
  beta_0 <- fit_data$beta_0
  home <- fit_data$home
  n_samples <- nrow(beta_0)

  y1 <- list()
  y2 <- list()

  for(i in 1:nrow(new_data)){
    h <- new_data[i, ]$h
    a <- new_data[i, ]$a

    if(!(model %in% special_models)){
      att <- fit_data$att
      def <- fit_data$def

      theta_1 <- beta_0 + home + att[, h] + def[, a]
      theta_2 <- beta_0 + att[, a] + def[, h]
    }

    if(model == "poisson"){
      y1_new <- rpois(n_samples, exp(theta_1))
      y2_new <- rpois(n_samples, exp(theta_2))
    }

    else if(model == "negbinom"){
      y1_new <- rnbinom(n_samples, mu=exp(theta_1), size=fit_data$phi_home)
      y2_new <- rnbinom(n_samples, mu=exp(theta_2), size=fit_data$phi_away)
    }

    else if(model == "poisson_infl"){
      p_zero_h <- fit_data$p_zero_h
      p_zero_a <- fit_data$p_zero_a

      y1_new <- rpois(n_samples, exp(theta_1)) * (1 - rbinom(n_samples, 1, (p_zero_h)))
      y2_new <- rpois(n_samples, exp(theta_2)) * (1 - rbinom(n_samples, 1, (p_zero_a)))
    }

    else if(model %in% special_models){
      att_h <- fit_data$att_h
      att_a <- fit_data$att_a
      def_h <- fit_data$def_h
      def_a <- fit_data$def_a

      theta_1 <- beta_0 + home + att_h[, h] + def_a[, a]
      theta_2 <- beta_0 + att_a[, a] + def_h[, h]

      if(model == "poisson_2"){
        y1_new <- rpois(n_samples, exp(theta_1))
        y2_new <- rpois(n_samples, exp(theta_2))
      }

      else if(model == "negbinom_2"){
        y1_new <- rnbinom(n_samples, mu=exp(theta_1), size=fit_data$phi_home)
        y2_new <- rnbinom(n_samples, mu=exp(theta_2), size=fit_data$phi_away)
      }

      else if(model == "poisson_infl_2"){
        p_zero_h <- fit_data$p_zero_h
        p_zero_a <- fit_data$p_zero_a

        y1_new <- rpois(n_samples, exp(theta_1)) * (1 - rbinom(n_samples, 1, (p_zero_h)))
        y2_new <- rpois(n_samples, exp(theta_2)) * (1 - rbinom(n_samples, 1, (p_zero_a)))
      }
    }

    y1[[paste0("y1_", i)]] <- y1_new
    y2[[paste0("y2_", i)]] <- y2_new
  }

  return(c(y1, y2, new_data))
}

#' Calculate the proportions of each outcome predicted by footstan::predict
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

#' Makes histograms for the number of goals predicted by footstan::predict
#'
#' @param prediction_data An object with the predictions made by the function footstan::predict
#' @param game Desired game in the new data
#' @param team Which team the user wants to visualize the estimated goals
#' @param home_name Name of the home team for visualization purposes.
#' @param away_name Name of the away team for visualization purposes.
#' @param fill_color Fill color for the histogram's bar.
#' @param bin_width Bin width for the histogram
#' @return A ggplot2 plot
#' @import ggplot2
#' @export


goals_histogram <- function(prediction_data, game, team=c("both", "home", "away"), home_team = "Home Team", away_team = "Away Team", fill_color = "blue", bin_width = 1){
  y <- 0
  graph_title <-  ""
  team <- team[1]
  if(team == "both"){
    y <- prediction_data[[paste0("y1_", game)]] + prediction_data[[paste0("y2_", game)]]
    graph_title <-  "Predicted frequency of goals by both teams"
  } else if(team == "home"){
    y <- prediction_data[[paste0("y1_", game)]]
    graph_title <- paste("Predicted frequency of goals by", home_team)
  } else {
    y <- prediction_data[[paste0("y2_", game)]]
    graph_title <-  paste("Predicted frequency of goals by", away_team)
  }

  ggplot(data = data.frame(y = y), aes(y)) +
    geom_histogram(binwidth = bin_width, fill = fill_color, color = "black", alpha = 0.7) +
    geom_text(stat = "count", aes(label = scales::percent(after_stat(count / sum(count)))), vjust = -0.5) +
    labs(title = graph_title, x = "", y = "") +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5)
    )
}


#' Plot the proportions of outcomes predicted by footstan::predict
#'
#' @param predictions An object with the predictions made by the function footstan::predict
#' @param team_names A data frame with ids and names to translate the ids into names to the plot
#' @return A ggplot2 plot
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom forcats fct_rev
#' @export

plot_predictions <- function(predictions, team_names = NULL){
  proportions <- footstan::prediction_proportions(predictions)

  if(!(is.null(team_names))){
    proportions$h <- team_names[which(team_names$id == proportions$h), ]$names
    proportions$a <- team_names[which(team_names$id == proportions$a), ]$names
    game_names <- paste(proportions$h, "X", proportions$a)
  } else {
    game_names <- paste("Team ", proportions$h, "X", "Team ", proportions$a)
  }

  proportions['game_id'] <- as.factor(1:nrow(proportions))
  proportions['game_name'] <- game_names

  proportions_long <- gather(proportions, key = "Variable", value = "Value", -game_id, -game_name, -h, -a)

  ggplot(proportions_long, aes(x = fct_rev(game_id), y = Value, fill = factor(Variable, levels = c("home_lost", "draw", "home_win")))) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = scales::percent(Value)), position = position_stack(vjust = 0.5), color = "white") +
    labs(title = "", x = "", y = "") +
    scale_fill_manual(values = c("home_lost" = "#e74c3c", "draw" = "#bdc3c7", "home_win" = "#2ecc71"),
                      labels = c("Home Team Victory", "Tie", "Home Team Defeat"),
                      limits = c("home_win", "draw", "home_lost")) +
    theme_minimal() +
    theme(legend.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_blank()) +
    scale_x_discrete(labels = rev(proportions_long$game_name)) +
    coord_flip()
}



