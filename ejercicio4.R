



library(magrittr)
library(ggfortify)


dir <- "C:\\Users\\julen\\OneDrive - International Organization for Migration - IOM\\Mater en Tecnicas de Investigación Social Aplicada (UAB)\\Master\\2. Cuantitativo\\4-Ana. Longitudinal\\Parte II Analisis y Tecnicas\\Practica"


data <- haven::read_spss(file.path(dir, "series demograficas.sav"))

data <- data %>%
  dplyr::mutate(
    date = trimws(fecha), .before = 1,
    year = as.numeric(gsub("^(\\d{4}).+", "\\1", date)),
    month = as.numeric(gsub("^.+M(\\d{2})$", "\\1", date)),
    date = as.Date(paste0(year, "-", month, "-", "15"))
  ) %>%
  dplyr::rename("births" = nacimientos, "marriages" = matrimonios) %>%
  dplyr::select(-c(year, month, fecha)) %>%
  dplyr::arrange(date)


# haven::write_sav(data, file.path(dir, "series demograficas formato.sav"))


################################ TS EXPLORATION ################################ 

#### EXPLORE SERIE FOR STATITIONARITY

# Plot serie en tendency line
p_ts_raw <- ggplot2::ggplot(data, ggplot2::aes(date, births, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::scale_x_date(breaks = "1 year", date_labels = "%Y-%m", minor_breaks = "1 year") +
  ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
# Serie is stationary, with decreasing tendency and seasonal cicle



#### CONFIRM WITH ACFs

# Autocorrelation functions: simple and partial
acf_raw <- lapply(
  c("correlation", "partial"),
  function(m) ggplot2::fortify(stats::acf(data$births, type = m, lag.max = 36))
)
names(acf_raw) <- c("simple_acf", "partial_acf")

# ACF enhance plot function
#' @param acf a list of stats::acf fortified with ggplot2::fortify
#' @return a list of enhanced autocorrelation plots

plot_acf <- function(acf) {

  p_acf_raw <- purrr::map(
    acf, 
    ~ .x %>% {
      ggplot2::ggplot(.x, ggplot2::aes(Lag, ACF)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_segment(ggplot2::aes(x = Lag, xend = Lag, y = 0, yend = ACF), size = 1) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_hline(yintercept = .$lower, linetype = "dashed", color = "red") +
        ggplot2::geom_hline(yintercept = .$upper, linetype = "dashed", color = "red") +
        ggplot2::scale_x_continuous(breaks = seq(0, max(.$Lag, na.rm = TRUE), 1)) +
        ggplot2::scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
        ggplot2::theme_bw()
    }
  )

  return(p_acf_raw)
}

plot_acf_raw <- plot_acf(acf_raw)





# Remove tendency 
data_ns1 <- data %>%
  dplyr::mutate(nacimientos_diffns1 = nacimientos - dplyr::lag(nacimientos, n = 1)) # wt= zt-zt-1


# Quitar la tendencia con diffenciacion no estacional 1
data %>%
  dplyr::mutate(
    nacimientos_diff1 = nacimientos - dplyr::lag(nacimientos)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(fecha, nacimientos_diff1, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_date(breaks = "1 year", date_labels = "%Y-%m") +
  ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
  















############################# TS ANALYSIS USING FORECAST #######################

install.packages(c("fable", "feasts", "forecast", "tseries", "tsibble", "zoo"))


# Create xts object (date as rownames, no characters in data, only numeric)
data_ts <- as.data.frame(data)
rownames(data_ts) <- data$date # date as rownames
data_ts <- xts::xts(data_ts[, -1], frequency = 12, order.by = as.Date(rownames(data_ts)))

# Plot series
forecast::autoplot(data_ts) +
  ggplot2::geom_smooth(method = "lm")

# plot autocorrelations
forecast::Acf(data_ts)

# Check diff: lags needed for non-stationary serie
forecast::ndiffs(data_ts)
forecast::nsdiffs(data_ts)

# differentiate: convert to non-stationary
xts::diff.xts(data_ts, lag = 1,  log = TRUE) %>%
  forecast::autoplot(.) +
  ggplot2::geom_smooth(method = "lm")


