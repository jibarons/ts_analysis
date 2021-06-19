
funs <- file.path("R", dir("R"))
sapply(funs, source)


install.packages(c("fable", "feasts", "forecast", "tseries", "tsibble", "zoo"))

library(magrittr)
library(dplyr)



data <- haven::read_spss("data/series_demograficas.sav")

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


################################# EXPLORE TS ################################### 

#### EXPLORE SERIE FOR STATITIONARITY

# Plot serie en tendency line
p_ts_raw <- ts_plot(data, date, births)
# SerieS is stationary, with decreasing tendency and seasonal cycle of 12 months

#### ASSES MODEL TYPE SUITABILITY

# Autocorrelation functions: simple and partial
acf_raw <- ts_acf(data$births, type = c("correlation", "partial"), plot = TRUE)
# Seems an hybrid model AR+MA would fit better. The acf plots show simple acf closer to
# AR(1) model with positive coefficient (likely to be high) and partial acf 
# closer to a MA model with negative coefficient


#### Test for Heterocedasticity

var_test_raw <- data %>%
  dplyr::mutate(year = format(date, format= "%Y"))
bartlett.test(var_test$births, var_test$year) # # p.value > 0.05 --> homocedastic
# Bartlett test is significant with p < 0.05, reject H0 homocedastic, it is heterocesdastic


################################### TRANSFORM TS ###############################

# # Guidelines (https://rpubs.com/riazakhan94/arima_with_example)
#
# It involves identifying three numbers p,d,q which specifies ARIMA model.
# 
# First we determine the value of d. We first check the stationarity of the data.
# If the data is stationary, then d=0. If the data is trendy then we take the first
# difference and check for stationarity again. We keep taking differences until we
# get a stationary output. The number of difference required is the number d.
# 
# There are different tools to detect stationarity. Visual observation of the data
# plot, auto correlation, variaogram are the most common.
# After determining d, we can utilize sample PACF (partial auto correlation) to
# get the AR order p. If the PACF cuts off after some lags, that number is the 
# order of AR.
# 
# We can determine the MA order q by looking at the sample ACF (auto correlation)
# of the differenced data. If the ACF cuts off after some lags, that number is the
# order of MA.
# 
# Theoretically, ACF of PACF cut off if one of the AR or MA orders is zero. If 
# both of them are non zero, then both ACF and PACF may exhibit damped sinusoid 
# and/or exponential decay. More works to do in these cases to indentify the model
# specifications.


#### REMOVE TENDENCY

# non-seasonal differentiation for various lags
lags_ns <- sapply(1:2, function(n) { # lags = 1:2

  diffns <- data %>%
    dplyr::mutate(
      births_diff = births - dplyr::lag(births, n = n),
      date_num = as.numeric(format(date, format= "%Y%m"))
    ) # wt= zt-zt-n
  
  suppressWarnings(print(ts_plot(diffns, date, births_diff)))

  cor(diffns$date_num, diffns$births_diff, use="complete.obs")

})
# lag = 1 show lower cor (removes tendency better) than lag = 2

# Transform data: remove tendency
data_trans_ns <- data %>%
  dplyr::mutate(births = births - dplyr::lag(births, n = 1))

p_ts_trans_ns <- ts_plot(data_trans_ns, date, births)


#### REMOVE SEASONAL CYCLE

# seasonal differentiation for various lags
lags_ss <- sapply(c(12, 24), function(n) { # lags = 12, 24
  
  data_difss <- data_trans_ns %>%
    dplyr::mutate(births_diff = births - dplyr::lag(births, n = n)) # wt= zt-zt-n
  
  suppressWarnings(print(ts_plot(data_difss, date, births_diff)))
  
})
# Plot for lag 12 seems to remove seasonal patterns

# Transform data: remove seasonality
data_trans_ns_ss <- data_trans_ns %>%
  dplyr::mutate(births = births - dplyr::lag(births, n = 12)) %>%
  dplyr::filter(!is.na(births))

p_ts_trans_ns_ss <- ts_plot(data_trans_ns_ss, date, births)

# test for heterocedasticity
var_test_trans_ns_ss <- data_trans_ns_ss %>%
  dplyr::mutate(year = format(date, format= "%Y"))
bartlett.test(var_test_trans_ns_ss$births, var_test_trans_ns_ss$year)


#### REMOVE HETEROCEDASTICITY

# Volatility for various logs
logs_var <- sapply(c(exp(1), 2, 10), function(n) { # trans = log
  
  data_difss <- data_trans_ns_ss %>%
    dplyr::mutate(
      births_diff = log(births, n), # wt= logn(zt)
      year = format(date, format= "%Y")
    ) %>%  
    dplyr::filter(!is.na(births_diff), !is.infinite(births_diff))
    
  print((nrow(data_trans_ns_ss) - nrow(data_difss)) / nrow(data_trans_ns_ss)) # % lost values after trans
  
  suppressWarnings(print(ts_plot(data_difss, date, births_diff)))
  
  bartlett.test(data_difss$births_diff, data_difss$year)[[3]] # p.value > 0.05 heterocedastic
  
})


# Volatility for various exp
exp_var <- sapply(c(0.1, 0.5, 0.9), function(n) { # trans = ^
  
  data_difss <- data_trans_ns_ss %>%
    dplyr::mutate(
      births_diff = births ^ n, # wt= zt^n
      year = format(date, format= "%Y")
    ) %>% 
    dplyr::filter(!is.na(births_diff), !is.infinite(births_diff))
  
  print((nrow(data_trans_ns_ss) - nrow(data_difss)) / nrow(data_trans_ns_ss)) # % lost values after trans
  
  suppressWarnings(print(ts_plot(data_difss, date, births_diff)))
  
  round(
    bartlett.test(data_difss$births_diff, data_difss$year)[[3]], 3 # p.value > 0.05 heterocedastic
  )
})

# Transformation with ^0.5 results in homocedastic serie (as per Bartlet's). However
# it losses > 50% of the sample in the trans. The serie has lost tendency and seasonal
# cycle so we continue without log or exp transformations


#### CHECK ACF OF TRANSFORMED DATA

p_ts_trans <- ts_plot(data_trans_ns_ss, date, births)

# Autocorrelation functions: simple and partial
acf_trans <- ts_acf(data_trans_ns_ss$births, type = c("correlation", "partial"))



############################ MODELING ARIMA ###################################

# Finding d 
# Following we plotted the original time series and the first difference. The 
# original plot shows a clear non stationarity. the acf does not cuts off, rather 
# it shows a slow decrease. On the other hand, the first difference data looks much
# stationary. The acf cuts off after some lags. We can perform other formal test to
# test the stationarity of the differenced data. But here from plot of the data and
# the acf plot provide good indication that the differenced data is stationary. 
# Since it took one differencing to get stationarity, here d=1
#
# Finding p and q
# The next step is to get the values of p and q, the order of AR and MA part. To 
# do so, we plotted the sample ACF and PACF of the first differenced data. We see 
# that the sample ACF cuts off after lag 2 and the sample PACF cuts off after lag 1.
# So we propose three ARMA models for the differenced data: ARMA(0,2), ARMA(1,0)
# and ARMA(1,2). That is, for the original time series, we propose three ARIMA 
# models, ARIMA(0,1,2), ARIMA(1,1,0) and ARMA(1,1,2).

# Select the model
# Guidance (https://otexts.com/fpp2/seasonal-arima.html; https://rpubs.com/riazakhan94/arima_with_example)
# Standard errors more than 2 standard deviation away from zero, then parameters
# passed the t-test
# Get the one with lower RMSE and lower AIC
# Check normality of residuals, should be close to normal. Can perform 
# Anderson Darling test. 


# Crete ts class
data_ts <- as.data.frame(data)
rownames(data_ts) <- data$date # date as rownames
data_ts <- xts::xts(data_ts[, "births"], frequency = 12, order.by = as.Date(rownames(data_ts)))





fits <- purrr::pmap(
  list(
    order = list(c(1, 1, 1), c(1, 2, 1)),
    seasonal = list(
      list(order = c(0, 1, 1), period = 12), 
      list(order = c(0, 1, 1), period = 12))
  ),
  ~ forecast::Arima(data_ts, order = ..1, seasonal = ..2)
)




forecast::autoplot(fits[[1]])


forecast::forecast(fits[[1]], h = 50) %>% 
  forecast::autoplot()

predict(fits[[1]], n.ahead = 25)


forecast::checkresiduals(fits[[1]])




# auto_fit <- forecast::auto.arima(data_ts, d = 2, D = 12, period = 12)
# 
# forecast::forecast(auto_fit, h = 50) %>% 
#   forecast::autoplot()












