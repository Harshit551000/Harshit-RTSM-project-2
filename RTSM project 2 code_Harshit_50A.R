# Required Packages
packages <- c('quantmod', 'car', 'forecast', 'tseries', 'FinTS', 'rugarch', 'utf8', 'ggplot2')

# Install all Packages with Dependencies
install.packages(packages, dependencies = TRUE)
# Load all Packages
lapply(packages, require, character.only = TRUE)

# Downloading stock data for Google
getSymbols(Symbols = 'GOOGL', 
           src = 'yahoo', 
           from = as.Date('2018-01-01'), 
           to = as.Date('2023-12-31'),
           periodicity = 'daily')

# Extract Adjusted Closing Price for Google
GOOGL_price <- na.omit(GOOGL$GOOGL.Adjusted) # Adjusted Closing Price
class(GOOGL_price) # xts (Time-Series) Object

# Calculate Returns
GOOGL_return <- na.omit(diff(log(GOOGL_price)))
plot(GOOGL_return)

# ADF test for Stationarity
adf_test_jj <- adf.test(GOOGL_return)
adf_test_jj

# Autocorrelation test
# Ljung-Box Test for Autocorrelation
lb_test_ds <- Box.test(GOOGL_return)
lb_test_ds

# ACF and PACF
acf(GOOGL_price) # ACF of Google Price Series
pacf(GOOGL_price) # PACF of Google Price Series

acf(GOOGL_return) # ACF of Google Return Series
pacf(GOOGL_return) # PACF of Google Return Series

# AutoARIMA
arma_pq_ds <- auto.arima(GOOGL_return)
arma_pq_ds

arma_pq <- auto.arima(GOOGL_price)
arma_pq

# ARIMA manipulation
arma13 <- arima(GOOGL_return, order = c(5, 0, 4))
arma13

ds_fpq <- forecast(arma13, h = 500)
plot(ds_fpq)

# Autocorrelation test
# Ljung-Box Test for Autocorrelation
lb_test_ds_A <- Box.test(arma13$residuals)
lb_test_ds_A

# Test for Volatility Clustering or Heteroskedasticity: Box Test 
stk_ret_sq <- arma13$residuals^2 
plot(stk_ret_sq)
stk_ret_sq_box_test <- Box.test(stk_ret_sq, lag = 10) 
stk_ret_sq_box_test 

# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
stk_ret_arch_test <- ArchTest(arma13$residuals, lags = 10) 
stk_ret_arch_test 

# GARCH model specification
garch_model1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE))
nse_ret_garch1 <- ugarchfit(garch_model1, data = arma13$residuals)
nse_ret_garch1

garch_model2 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,5), include.mean = FALSE))
nse_ret_garch2 <- ugarchfit(garch_model2, data = arma13$residuals)
nse_ret_garch2

# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
gar_resd <- residuals(nse_ret_garch2)^2
stk_ret_arch_test1 <- ArchTest(gar_resd, lags = 1)
stk_ret_arch_test1

# GARCH Forecast
stk_ret_garch_forecast1 <- ugarchforecast(nse_ret_garch2, n.ahead = 50)
stk_ret_garch_forecast1

plot(stk_ret_garch_forecast1)
1
