
##################################### SET UP PROJECT ###########################
########################### SET-UP PACKAGES ####################################

start.all <- Sys.time()

if (!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

if(packageVersion("dplyr") < "1.0.0") {install.packages("dplyr")}

## Install and load packages
check.packages <- function(pkg){
  ## Function description: credit to Danielle Smith
  ## https://gist.github.com/smithdanielle/9913897 . This function checks if a 
  ## specific list of packages are installed and if not, installs them. It then
  ## loads them.  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))  
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}

packages.project <- c(
  "forecast", "tseries", "tsibble", "zoo", "ggfortify", "kableExtra", "sweep", "rmarkdown",
  "ggplot2", "purrr", "knitr", "haven", "scales", "tidyr", "magrittr"
)

check.packages(packages.project)


## Load project functions

funs <- file.path("R", dir("R"))
sapply(funs, source)


# Load data
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


#### Test for Heterocedasticit

var_test_raw <- data %>%
  dplyr::mutate(year = format(date, format= "%Y"))
het_test_raw <- bartlett.test(var_test_raw$births, var_test_raw$year) # # p.value > 0.05 --> homocedastic.

het_raw <- as.data.frame(unlist(het_test_raw)) %>%
  dplyr::mutate_all(~round(as.numeric(.), 5)) %>%
  dplyr::rename_with(~ "value", dplyr::matches("unlist")) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(parameter = rownames(.), .before = 1)
rownames(het_raw) <- NULL


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
lags_ns <- lapply(1:2, function(n) { # lags = 1:2

  out <- c()
  out[[paste0("lag", n)]][["data"]] <- data %>%
    dplyr::mutate(
      births = births - dplyr::lag(births, n = n), # wt= zt-zt-n
      date_num_temp = as.numeric(format(date, format= "%Y%m")) # temp var for cor
    ) # wt= zt-zt-n
  
  out[[paste0("lag", n)]][["plot"]] <- ts_plot(
    out[[paste0("lag", n)]][["data"]], date, births
  )
  out[[paste0("lag", n)]][["coef"]] <- cor(
    out[[paste0("lag", n)]][["data"]]$date_num_temp, out[[paste0("lag", n)]][["data"]]$births,
    use="complete.obs"
  )
  
  out[[paste0("lag", n)]][["data"]] <- out[[paste0("lag", n)]][["data"]] %>%
    dplyr::select(-date_num_temp) %>% # remove temp var
    dplyr::filter(!is.na(births))
    
  unlist(out, recursive = FALSE)

})
# lag = 1 show lower cor (removes tendency better) than lag = 2

# Transform data: remove tendency
data_trans_ns <- lags_ns[[1]][["lag1.data"]]


#### REMOVE SEASONAL CYCLE

# seasonal differentiation for various lags
lags_ss <- lapply(c(12, 24), function(n) { # lags = 12, 24
  
  out <- c()
  out[[paste0("lag", n)]][["data"]] <- data_trans_ns %>%
    dplyr::mutate(
      births = births - dplyr::lag(births, n = n), # wt= zt-zt-n
    ) 
  
  out[[paste0("lag", n)]][["plot"]] <- ts_plot(
    out[[paste0("lag", n)]][["data"]], date, births
  )
  
  out[[paste0("lag", n)]][["data"]] <- out[[paste0("lag", n)]][["data"]] %>%
    dplyr::filter(!is.na(births))
  
  unlist(out, recursive = FALSE)
  
})
# Check simple ACF for seasonality trend
ts_acf(lags_ss[[1]][["lag12.data"]]$births, type = c("correlation"), plot = TRUE)
# Plot for lag 12 seems to remove seasonal patterns and implies less data lost

# Transform data: remove seasonality
data_trans_ns_ss <- lags_ss[[1]][["lag12.data"]]

var_test_trans_ns_ss  <- data_trans_ns_ss %>%
  dplyr::mutate(year = format(date, format= "%Y"))
het_test_trans_ns_ss <- bartlett.test(var_test_trans_ns_ss$births, var_test_trans_ns_ss$year) # # p.value > 0.05 --> homocedastic.

het_trans_ns_ss  <- as.data.frame(unlist(het_test_trans_ns_ss)) %>%
  dplyr::mutate_all(~round(as.numeric(.), 5)) %>%
  dplyr::rename_with(~ "value", dplyr::matches("unlist")) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(parameter = rownames(.), .before = 1)
rownames(het_trans_ns_ss ) <- NULL


# #### REMOVE HETEROCEDASTICITY
# 
# # Volatility for various logs
# logs_var <- lapply(c(exp(1), 2, 10), function(n) { # trans = log
#   
#   out <- c()
#   
#   out[[paste0("trans", n)]][["data"]] <- data_trans_ns_ss %>%
#     dplyr::mutate(
#       births = log(births, n), # wt= logn(zt)
#       year_temp = format(date, format= "%Y")
#     ) %>%  
#     dplyr::filter(!is.na(births), !is.infinite(births))
# 
#   print(# % lost values after trans
#     (nrow(data_trans_ns_ss) - nrow(out[[paste0("trans", n)]][["data"]])) / nrow(data_trans_ns_ss)
#   ) 
# 
#   out[[paste0("trans", n)]][["plot"]] <- ts_plot(
#     out[[paste0("trans", n)]][["data"]], date, births
#   )
#   
#   out[[paste0("trans", n)]][["coef"]] <- bartlett.test(
#     out[[paste0("trans", n)]][["data"]]$births, out[[paste0("trans", n)]][["data"]]$year_temp
#   )[[3]]
#   
#   out[[paste0("trans", n)]][["data"]] <- out[[paste0("trans", n)]][["data"]] %>%
#     dplyr::select(-year_temp) # Remove temp var
#   
#   unlist(out, recursive = FALSE)
#   
# })
# 
# 
# # Volatility for various exp
# exp_var <- sapply(c(0.1, 0.5, 0.9), function(n) { # trans = ^
#   
#   data_difss <- data_trans_ns_ss %>%
#     dplyr::mutate(
#       births_diff = births ^ n, # wt= zt^n
#       year = format(date, format= "%Y")
#     ) %>% 
#     dplyr::filter(!is.na(births_diff), !is.infinite(births_diff))
#   
#   print((nrow(data_trans_ns_ss) - nrow(data_difss)) / nrow(data_trans_ns_ss)) # % lost values after trans
#   
#   suppressWarnings(print(ts_plot(data_difss, date, births_diff)))
#   
#   round(
#     bartlett.test(data_difss$births_diff, data_difss$year)[[3]], 3 # p.value > 0.05 heterocedastic
#   )
# })

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
# passed the t-test (i.e. coef - se far away from 0)
# Get the one with lower RMSE and lower AIC
# Check normality of residuals, should be close to normal. Can perform 
# Anderson Darling test. 

predict_n <- 50

# Crete ts class
data_ts <- as.data.frame(data)
rownames(data_ts) <- data$date # date as rownames
data_ts <- xts::xts(data_ts[, "births"], frequency = 12, order.by = as.Date(rownames(data_ts)))


# Define models to test
models <- list(
  order = list(c(2, 1, 3), c(2, 1, 2), c(3, 1, 2), c(2, 1, 3)),
  seasonal = list(
    list(order = c(0, 1, 1), period = 12), 
    list(order = c(0, 1, 1), period = 12),
    list(order = c(0, 1, 1), period = 12),
    list(order = c(0, 1, 2), period = 12)
  )
)


# Test models
fits <- purrr::pmap(models, ~ forecast::Arima(data_ts, order = ..1, seasonal = ..2))


# Get tidy summary
fits_summ <- dplyr::bind_rows(lapply(fits, sweep::sw_glance))


## Get tidy coef with ci
fits_coef_stat <- lapply(fits, coef)
fits_coef_ci <- lapply(fits, confint)
fits_coef <- purrr::map2(fits_coef_stat, fits_coef_ci, cbind)

fits_coef <- purrr::map(
  fits_coef, 
  ~ dplyr::mutate(as.data.frame(.x), parameter = rownames(.x), .before = 1) %>%
    dplyr::rename("coeficient" = V1)
)
names(fits_coef) <- fits_summ$model.desc
fits_coef <- dplyr::bind_rows(fits_coef, .id = "model.desc")
rownames(fits_coef) <- NULL


# Check residuals (Only for best fitted)
fits_residuals <- sweep::sw_tidy(forecast::checkresiduals(fits[[1]])) %>%
  select(method, dplyr::everything())
p_resid_acf <- recordPlot()


# Predict
fit_predict <- forecast::forecast(fits[[1]], h = predict_n)

p_predict <- forecast::autoplot(fit_predict) +
  ggplot2::scale_x_continuous(breaks = seq(0, 440, 20)) +
  ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))



#### REPORT

#write.csv(fit_predict, "predicted_values.csv", row.names = FALSE)
rmarkdown::render("report.Rmd")




