# Green model test script==========================================================

# Green data consists of temp, Co2 and sea levels. 
# Models to be ran : VAR (only of lag 1 to prevent overfitting), ARIMA-X and
# VAR-X (where the exogenous variable is sea levels)

library(tidyverse)
library(fpp2)
library(urca)
library(TSstudio)

# Load data for absolute temp ===================================================
green_df <- read_csv("C:/Users/rayne/Documents/GitHub/DSA301_TimeSeries/Data/green_data_pc_new.csv")|>
  rename(avg_temp=AvgTemp)

green_ts <- ts(green_df[,3:5], start=c(1880,1), end=c(2013,1))

# Try NASA temp change data instead (doesnt work)=================================
# Change in temp data
# This doesnt work because boxcox transformation (as part of ARIMA) does not work
# on negative integers, and change in temp can be negative

nasa_df <- read_csv("C:/Users/rayne/Documents/GitHub/DSA301_TimeSeries/Data/NASA_temp_data.csv", skip=1)
nasa_df <- nasa_df |>
  dplyr::select(-c(`J-D`,`D-N`,DJF,MAM,JJA,SON))

nasa_df <- nasa_df %>%
  mutate_at(vars(-Year), as.numeric)

nasa_df <- nasa_df %>%
  mutate(avg_temp = rowMeans(dplyr::select(., -1))) %>%  # Exclude the first column (year) from the calculation
  dplyr::select(Year, avg_temp)|>
  rename(year_ref=Year)

green_df <- green_df |> 
  dplyr::select(-avg_temp)|>
  left_join(nasa_df, by="year_ref")

green_ts <- ts(green_df[,3:5], start=c(1880,1), end=c(2013,1))

# Split train test =====================================================================
outofsample=26
green_model_ts_split <- ts_split(green_ts, sample.out=outofsample)

# VAR select to tell us optimal order of VAR lags
vars::VARselect(green_model_ts_split$train[,1:3])
# Based on BIC, 1 lag is optimal.

# Lag 1 VAR ========================================================================
green_var1 <- vars::VAR(green_model_ts_split$train[,1:3], p=1, type="const")
var_fc <- forecast::forecast(green_var1, h=outofsample)
accuracy(x=green_model_ts_split$test[,3], var_fc$forecast$avg_temp)

# Test rmse: 0.25667, Test MAPE: 1.18585

# Test for VAR legitimacy
green_var1 |> 
  vars::serial.test() # p-value = 0.9976, passes

# Making all variables stationary for granger causality test

# column 1 sea lvl, column 2 CO2, column 3 temp

# Granger casuality test
vars::causality(green_var1, cause = 'sea_level')$Granger
vars::causality(green_var1, cause = 'carbon_emissions_per_capita')$Granger
vars::causality(green_var1, cause = 'avg_temp')$Granger


# VAR-X =========================================================================
# VAR-X with temp and C02 as interaction variables, and exogenous sea level

vars::VARselect(green_model_ts_split$train[,2:3]) # BIC says 1 lag is optimal

green_varx <- vars::VAR(green_model_ts_split$train[,2:3], p=1, type="const", exogen = green_model_ts_split$train[,1])

# Making exogenous variable a matrix for dumvar argument in predict() fxn
exog_for_forecasting <- matrix(green_model_ts_split$test[,1], ncol = 1)
varx_fc <- predict(green_varx, n.ahead=outofsample, dumvar=exog_for_forecasting)

# Converting the VAR-X forecast back into time series object, for comparison with time series test data, getting the accuracy.

varx_fc1 <- ts(varx_fc$fcst$avg_temp, start=c(1988,1), end=c(2013,1))
accuracy(x=green_model_ts_split$test[,3], varx_fc1)

# Test RMSE: 0.2766112 MAPE: 1.270304

# ARIMA-X =======================================================================
green_arimax <- auto.arima(green_model_ts_split$train[,3], lambda="auto", xreg=green_model_ts_split$train[,1:2])
# sea_level t stat:1.5968/1.1846 = 1.347966 < 1.96, hence insignificant
# carbon_emission_per_capita t stat: 0.0622/0.0788 = 0.7893, hence insignificant

green_arimax_fc <- forecast::forecast(green_arimax, h=outofsample, xreg=green_model_ts_split$train[,1:2])
accuracy(x=green_model_ts_split$test[,3], green_arimax_fc)
# Test RMSE: 1.331383 MAPE:9.7220382

# Baseline ARIMA
green_arima <- auto.arima(green_model_ts_split$train[,3], lambda="auto")
green_arima_fc <- forecast::forecast(green_arimax, h=outofsample)
accuracy(x=green_model_ts_split$test[,3], green_arima_fc)

# Test RMSE: 0.33866 MAPE: 1.56338