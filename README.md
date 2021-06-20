## Introduction

We fitted an ARIMA model in the series of **births in Catalunya from February 1975 to December 2006**. The series has a monthly periodicity comprising a total of 384 observations. We first performed an exploration of the series assessing for stationary; afterwards we performed a series of transformation on the series to achieve an stationary series, which helped us to define the parameters for the model; finally, with the ifnromation of the previous steps, we fitted an ARIMA model for prediction of the next 50 periods. Note the purpose of this work was to show the logic of a time series analysis. 

we confirm that the model that we expected from our exploratory analysis **ARIMA(2,1,3)(0,1,1)[12]** was the one that offers the best fit among the four models we have tested. It resulted in an **RMSE of 848.332** and **AIC of 6,081.491**. Furthermore, we observe that the coefficients confidence interval does not include 0, thus all becoming significant and supporting a good fit.

Two functions have been developed ad-hoc for this project to enhance the outputs for Autocorrelation functions and time series sequencing visualization:

1. `ts_acf` allows enhance the outputs of stats::acf for multiple ACF types, providing tidy tables and ggplots 
```r
ts_acf(ts, type = c("correlation", "partial"), plot = TRUE, linetype = "dashed", color = "red")

#' @param ts a vector with he series to autocorrelate
#' @param type a character vector with type of correlations, passed to stats::acf
#' @param out a string with desired output c("tables", "plots")
#' @param linetype a string passed to ggplot2::geom_hline
#' @param color a string passed to ggplot2::geom_hline
#' 
#' @return a list of enhanced autocorrelation table or plots

```

2. `ts_plot` provides an enhanced ggplot for the times series with tendency line and improved scales for population series
```r
ts_plot(data, date, ts)

#' @param data a data frame
#' @param date a date type vector
#' @param ts a numeric vector with the time serie to plot
#' 
#' @return a plot of the ts
```
