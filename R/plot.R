#' Plot diagnostic graphics for the MCMC chains
#'
#' @param fit A footstanfit object.
#' @param params A character vector with the desired parameters to be ploted
#' @return A GGplot plot
#' @import gridExtra
#' @import ggplot2
#' @import grid
#' @export
#'

visual_diagnostic <- function(fit, params){
  for(param in params){
    dens_plot <- dens_plot(fit, param)
    autocorrelation <- acf_plot(fit, param)
    trace_plot <- trace_plot(fit, param)
    ergotic_p <- ergotic_plot(fit, param)
    plot_layout <- rbind(c(1, 2),
                         c(1, 3),
                         c(4, 4))

    num_chains <- length(dens_plot)

    title <- textGrob(param, gp = gpar(fontsize = 16, fontface = "bold"))

    for(i in 1:num_chains){
      p <- gridExtra::grid.arrange(dens_plot[[i]], autocorrelation[[i]],
                                   ergotic_p[[i]], trace_plot[[i]],
                                   layout_matrix = plot_layout,
                                   top = title)
      p

      readline(prompt = "Hit <Return> to see the next plot: ")
    }
  }
}

#' Plot ergotic mean of a param
#'
#' @param fit A footstanfit object.
#' @param param Desired param to be graphed out
#' @return A GGplot plot
#' @importFrom ggplot2 ggplot geom_point labs theme
#' @importFrom rstan extract
#' @import posterior
#' @export
#'

ergotic_plot <- function(fit, param) {
  draws <- posterior::as_draws(fit)
  param_data <- data.frame(posterior::subset_draws(draws, variable = param))

  n <- nrow(param_data)
  colnames(param_data) <- paste0("chain ", 1:ncol(param_data))

  result <- apply(param_data, 2, function(x) {
    sapply(seq_along(x), function(i) {
      mean(x[1:i])
    })
  })

  param_data <- as.data.frame(result)
  param_data$index <- 1:n

  plot_list <- list()

  # Generate plots for each chain using lapply
  plot_list <- lapply(1:(ncol(param_data) - 1), function(i) {
    chain <- paste0("chain ", i)
    ggplot(param_data, aes(x = index, y = .data[[chain]])) +
      geom_line(color = "#689ab3") +
      labs(title = paste0("Ergotic mean plot for ", chain), x = "", y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  })

  return(plot_list)
}

#' Plot the draws for each chain
#'
#' @param fit A footstanfit object.
#' @param param Desired param to be graphed out
#' @return A GGplot plot
#' @importFrom ggplot2 ggplot geom_point labs theme
#' @importFrom rstan extract
#' @import posterior
#' @export
#'

trace_plot <- function(fit, param) {
  draws <- posterior::as_draws(fit)
  param_data <- data.frame(posterior::subset_draws(draws, variable = param))

  n <- nrow(param_data)
  colnames(param_data) <- paste0("chain ", 1:ncol(param_data))
  param_data$index <- 1:n

  plot_list <- list()

  # Generate plots for each chain using lapply
  plot_list <- lapply(1:(ncol(param_data) - 1), function(i) {
    chain <- paste0("chain ", i)
    ggplot(param_data, aes(x = index, y = .data[[chain]])) +
      geom_line(color = "#689ab3") +
      labs(title = paste0("Traceplot for ", chain), x = "", y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  })

  return(plot_list)
}


#' Plot the distribution of the draws for each chain
#'
#' @param fit A footstanfit object.
#' @param param Desired param to be graphed out
#' @return A GGplot plot
#' @importFrom ggplot2 ggplot geom_point labs theme
#' @importFrom rstan extract
#' @import posterior
#' @export
#'

dens_plot <- function(fit, param) {
  draws <- posterior::as_draws(fit)
  param_data <- data.frame(posterior::subset_draws(draws, variable = param))

  n <- nrow(param_data)
  colnames(param_data) <- paste0("chain ", 1:ncol(param_data))

  plot_list <- list()

  # Generate plots for each chain using lapply
  plot_list <- lapply(1:(ncol(param_data)), function(i) {
    chain <- paste0("chain ", i)
    ggplot(param_data, aes(x = .data[[chain]])) +
      geom_density(fill = "#689ab3", alpha = 0.7) +
      labs(title = paste0("Density plot for ", chain), x = "", y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  })

  return(plot_list)
}


#' Plot the autocorrelation for each chain
#'
#' @param fit A footstanfit object.
#' @param param Desired param to be graphed out
#' @return A GGplot plot
#' @importFrom ggplot2 ggplot geom_point labs theme
#' @importFrom rstan extract
#' @import posterior
#' @export
#'

acf_plot <- function(fit, param) {
  draws <- posterior::as_draws(fit)
  param_data <- data.frame(posterior::subset_draws(draws, variable = param))

  n <- nrow(param_data)
  colnames(param_data) <- paste0("chain ", 1:ncol(param_data))

  plot_list <- list()

  # Generate plots for each chain using lapply
  plot_list <- lapply(1:(ncol(param_data)), function(i) {
    chain <- paste0("chain ", i)
    acf_data <- stats::acf(param_data[[chain]], plot = F)
    acf_df <- data.frame(
      Lag = acf_data$lag,
      ACF = acf_data$acf
    )
    ggplot(acf_df, aes(x = Lag, y = ACF)) +
      geom_col(fill = "#689ab3", width = 0.7) +
      geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
      geom_hline(yintercept = 0.2, color = "red", alpha = 0.5, linetype = "dashed") +
      geom_hline(yintercept = -0.2, color = "red", alpha = 0.5, linetype = "dashed") +
      labs(title = paste0("Autocorrelation for ", chain), x = "", y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  })

  return(plot_list)
}
