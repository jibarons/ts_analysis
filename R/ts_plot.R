
#' @description Enhace time series plot
#' 
#' @param data a data frame
#' @param date a date type vector
#' @param ts a numeric vector with the time serie to plot
#' 
#' @return a plot of the ts
#' 

ts_plot <- function(data, date, ts) {
  

  p <- ggplot2::ggplot(data, ggplot2::aes({{date}}, {{ts}}, group = 1)) +
    ggplot2::geom_line(size = 0.25) +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::scale_x_date(breaks = "1 year", date_labels = "%Y-%m", minor_breaks = "1 year") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    # ggplot2::labs(title = paste(
    #   rlang::as_name(rlang::enquo(ts)), "by", rlang::as_name(rlang::enquo(date))
    # )) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
 
  return(p)
  
}

