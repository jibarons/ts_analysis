#'
#' @description Enhance outputs of stats::acf
#' 
#' @param ts a vector with he series to autocorrelate
#' @param type a character vector with type of correlations, passed to stats::acf
#' @param out a string with desired output c("tables", "plots")
#' @param linetype a string passed to ggplot2::geom_hline
#' @param color a string passed to ggplot2::geom_hline
#' 
#' @return a list of enhanced autocorrelation table or plots
#' 

ts_acf <- function(ts, type = c("correlation", "partial"), plot = TRUE, 
                     linetype = "dashed", color = "red") {

  acf <- lapply(
    type,
    function(m) ggplot2::fortify(stats::acf(ts, type = m, lag.max = 36, plot = FALSE)) # needs `ggfortify` to coerce stats::acf
  )
  names(acf) <- type
  
    
  if (plot) {
    
    i <- 0
    
    p_acf <- purrr::map2(
      acf, type, 
      ~ .x %>% {
        ggplot2::ggplot(.x, ggplot2::aes(Lag, ACF)) +
          ggplot2::geom_point(size = 1.5) +
          ggplot2::geom_segment(ggplot2::aes(x = Lag, xend = Lag, y = 0, yend = ACF), size = 0.75) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::geom_hline(yintercept = .$lower, linetype = linetype, color = color) +
          ggplot2::geom_hline(yintercept = .$upper, linetype = linetype, color = color) +
          ggplot2::scale_x_continuous(breaks = seq(0, max(.$Lag, na.rm = TRUE), 1)) +
          ggplot2::scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
          # ggplot2::labs(title = .y) +
          ggplot2::theme_bw()
      }
    )
    
    acf <- list(tables = acf, plots = p_acf)
    
  }

  return(acf)
}