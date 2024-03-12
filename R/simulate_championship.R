#' Simulate a single championship
#'
#' @param num_teams Number of teams in the championship.
#' @param model Which model to use to simulate the data
#' @param params Model parameters to simulate the data
#'
#' @return A data frame where each row it's a game, with the teams that are playing and the result of the game
#' @export
#'
#' @importFrom stats rnorm rbinom rpois rnbinom

simulate_championship <- function(num_teams, model, params){
  if(model != "poisson_2"){
    sd_att <- params$sd_att
    sd_def <- params$sd_def
  }

  beta_0 <- params$beta_0
  home <- params$home

  games <- data.frame(
    h = rep(1:num_teams, each = num_teams),
    a = rep(1:num_teams, times = num_teams)
  )

  games <- games[games$h != games$a, ]
  num_games <- length(games$h)

  home_team <- games$h
  away_team <- games$a

  if(model != "poisson_2"){
    att_effects <- rnorm(num_teams, 0, sd_att)
    def_effects <- rnorm(num_teams, 0, sd_def)

    theta_1 <- beta_0 + home + att_effects[home_team] + def_effects[away_team]
    theta_2 <- beta_0 + att_effects[away_team] + def_effects[home_team]
  }

  if(model == "poisson"){
    y1 <- rpois(num_games, exp(theta_1))
    y2 <- rpois(num_games, exp(theta_2))
  }

  else if(model == "negbinom"){
    phi_home <- params$phi_home
    phi_away <- params$phi_away

    y1 <- rnbinom(num_games, mu=exp(theta_1), size=phi_home)
    y2 <- rnbinom(num_games, mu=exp(theta_2), size=phi_away)
  }

  else if(model == "poisson_infl"){
    p_zero_h <- params$p_zero_h
    p_zero_a <- params$p_zero_a

    y1 <- rpois(num_games, exp(theta_1)) * (1 - rbinom(num_games, 1, (p_zero_h)))
    y2 <- rpois(num_games, exp(theta_2)) * (1 - rbinom(num_games, 1, (p_zero_a)))
  }

  else if(model == "poisson_2"){
    sd_att_h <- params$sd_att_h
    sd_att_a <- params$sd_att_a
    sd_def_h <- params$sd_def_h
    sd_def_a <- params$sd_def_a

    att_h_effects <- rnorm(num_teams, 0, sd_att_h)
    att_a_effects <- rnorm(num_teams, 0, sd_att_a)

    def_h_effects <- rnorm(num_teams, 0, sd_def_h)
    def_a_effects <- rnorm(num_teams, 0, sd_def_a)

    theta_1 <- beta_0 + home + att_h_effects[home_team] + def_a_effects[away_team]
    theta_2 <- beta_0 + att_a_effects[away_team] + def_h_effects[home_team]

    y1 <- rpois(num_games, exp(theta_1))
    y2 <- rpois(num_games, exp(theta_2))
  }

  games$y1 <- y1
  games$y2 <- y2

  return(games)
}
