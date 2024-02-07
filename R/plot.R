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
    plot_layout <- rbind(c(1, 2),
                         c(3, 3))

    p <- gridExtra::grid.arrange(dens_plot, autocorrelation,
                                 trace_plot,
                                 layout_matrix = plot_layout)
    p
  }
}
