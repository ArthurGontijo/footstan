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
  sd_att <- params$sd_att
  sd_def <- params$sd_def
  beta_0 <- params$beta_0
  home <- params$home

  games <- data.frame(
    h = rep(1:num_teams, each = num_teams),
    a = rep(1:num_teams, times = num_teams)
  )
  games <- games[games$h != games$a, ]
  num_games <- length(games$h)

  att_effects <- rnorm(num_teams, 0, sd_att)
  def_effects <- rnorm(num_teams, 0, sd_def)

  home_team <- games$h
  away_team <- games$a

  theta_1 <- beta_0 + home + att_effects[home_team] + def_effects[away_team]
  theta_2 <- beta_0 + att_effects[away_team] + def_effects[home_team]

  if(model == "poisson"){
    y1 <- rpois(num_games, exp(theta_1))
    y2 <- rpois(num_games, exp(theta_2))
  }

  else if(model == "negbinom"){
    phi_att <- params$phi_att
    phi_def <- params$phi_def

    y1 <- rnbinom(num_games, mu=exp(theta_1), size=phi_att)
    y2 <- rnbinom(num_games, mu=exp(theta_2), size=phi_def)
  }

  else if(model == "poisson_infl"){
    p_zero_h <- params$p_zero_h
    p_zero_a <- params$p_zero_a

    y1 <- rpois(num_games, exp(theta_1)) * (1 - rbinom(num_games, 1, (p_zero_h)))
    y2 <- rpois(num_games, exp(theta_2)) * (1 - rbinom(num_games, 1, (p_zero_a)))
  }

  games$y1 <- y1
  games$y2 <- y2

  return(games)
}
