#' Simulate n different championships
#'
#' @param simulations Number of simulations to generate
#' @param num_teams Number of teams in the championship.
#' @param model Which model to use to simulate the data
#' @param params Model parameters to simulate the data
#'
#' @return A list whose each element corresponds to a simulated championship
#' @export
#'

generate_sample_data <- function(simulations, num_teams, model, params){

  generated_data <- lapply(1:simulations,
                          function(i) simulate_championship(num_teams = num_teams,
                                                            model = model,
                                                            params = params))

  return(generated_data)
}
