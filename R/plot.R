#' Plot diagnostic graphics for the MCMC chains
#'
#' @param fit A footstanfit object.
#' @param params Desired params to be graphed out
#' @return A GGplot plot
#' @import gridExtra
#' @importFrom ggplot2 ylab
#' @importFrom bayesplot mcmc_dens mcmc_acf_bar mcmc_trace
#' @export
#'

visual_diagnostic <- function(fit, params){
  for(param in params){
    dens_plot <- bayesplot::mcmc_dens(fit, pars = param) + ylab("Density")
    autocorrelation <- bayesplot::mcmc_acf_bar(fit, pars = param)
    trace_plot <- bayesplot::mcmc_trace(fit, pars = param)
    ergotic_p <- ergotic_plot(fit, param)
    plot_layout <- rbind(c(1, 2),
                         c(1, 3),
                         c(4, 4))

    p <- gridExtra::grid.arrange(dens_plot, autocorrelation, ergotic_p,
                                 trace_plot,
                                 layout_matrix = plot_layout)
    p
  }
}

#' Plot ergotic mean of a param
#'
#' @param fit A footstanfit object.
#' @param params Desired params to be graphed out
#' @return A GGplot plot
#' @importFrom ggplot2 ggplot geom_point labs theme
#'

ergotic_plot <- function(fit, param) {
  param_data <- extract(fit)[[param]]
  n <- length(param_data)
  ergotic_mean <- sapply(1:n, function(i) sum(param_data[1:i]) / i)
  plot_data <- data.frame(index = 1:n, value = ergotic_mean)
  p <- ggplot(plot_data, aes(x = index, y = value)) +
        geom_line(color = "#689ab3") +
        labs(title = "Ergodic Mean", x = "", y = "") +
        theme(plot.title = element_text(hjust = 0.5, vjust = 0.2, size = 12))

  return(p)
}

