library(tidyverse)
library(fpp2)
library(urca)

#Q1 ===============================================================================
# Data cleaning 
nat_gas_df <- read_csv("Data/NG_CONS_SUM_DCU_NUS_M.csv")
nat_gas_df<- nat_gas_df |>
  select(Date, `Volumes Delivered to Consumers`, Residential, Commercial, Industrial,`Vehicle Fuel`, `Electric Power`)|>
  mutate(Date=my(Date))|>
  filter(Date > ymd("2000-12-01"))

nat_gas_ts <- ts(nat_gas_df["Volumes Delivered to Consumers"],
                 start=c(2001,1), 
                 end=c(2021,12),
                 deltat=1/12)

# Removing determinant variables
nat_gas_ts <- nat_gas_ts/monthdays(nat_gas_ts)

# Simple plot
autoplot(nat_gas_ts)

# ETS decomposition
decompose(nat_gas_ts)|>
  autoplot()

# Get trend component
nat_gas_trend <- trendcycle(decompose(nat_gas_ts))

nat_gas_trend|> 
  autoplot()

# How many order of differencing needed?
nsdiffs(nat_gas_trend) #0

ndiffs(nat_gas_trend) #1

# 1 order diff of trendcycle
nat_gas_trend <- diff(nat_gas_trend)
nat_gas_trend|>
  autoplot()

# kpss test to see if data is now stationary
summary(ur.kpss(nat_gas_trend))
# Since test stat 0.18<critical value at 5% 0.463, we accept H0, data is stationary

# Seasonal component
nat_gas_season <- seasonal(decompose(nat_gas_ts))

# Take a look
autoplot(nat_gas_season)

# snaive model on s
nat_gas_season_model <- snaive(nat_gas_season, h=10)

# Benchmark models on nat_gas_trend
naive(nat_gas_trend)
snaive(nat_gas_trend)
rwf(nat_gas_trend, drift=TRUE)
meanf(nat_gas_trend)

# Define fxn for goodness-of-fit analysis on the different models
benchmark_model_results <- function(benchmark_model){
  model_forecast <- forecast(benchmark_model)
  accuracy(model_forecast)
}

# Seasonal snaive model
benchmark_model_results(nat_gas_season_model)

# trend models
benchmark_model_results(naive(nat_gas_trend))
benchmark_model_results(snaive(nat_gas_trend))
benchmark_model_results(rwf(nat_gas_trend, drift=TRUE))
benchmark_model_results(meanf(nat_gas_trend))
# rwf has lowest RMSE and MAPE, and hence is the optimal model for trendcycle
nat_gas_trend_model <- rwf(nat_gas_trend, drift=TRUE)
  
checkresiduals(nat_gas_trend_model$residuals+nat_gas_season_model$residuals)
# p value <0.05 so we reject H0, there is still time series info aka autocorrelation 
# in the model.


#Q2 ===============================================================================
SPY_df <- read_csv("Data/SPYmonthly2003.csv")

SPY_df <-  SPY_df |> 
  select(Date, Open) #columns Date and Open selected

SPY_ts <- ts(SPY_df["Open"],
             start= c(2004,01),
             end = c(2024, 01),
             deltat = 1/12)

acf(SPY_ts)
ggseasonplot(SPY_ts)
autoplot(decompose(SPY_ts))
autoplot(SPY_ts)
# There is trend - slowly decaying acf. But no seasonality, especially as ggseasonplot() 
# shows different lines for different years.

# BoxCox
autoplot(BoxCox(SPY_ts,BoxCox.lambda(SPY_ts)))
# Boxcox not very useful bc the data is still not stationary... smh all these useless methods

# Calendar corrections not needed as data is recorded for the first day and last day of month 

# Percentage change
diff(SPY_ts)/stats::lag(SPY_ts, k =-1)*100

autoplot(diff(SPY_ts)/stats::lag(SPY_ts, k =-1)*100)

# kpss test for stationarity
ur.kpss(diff(SPY_ts)/stats::lag(SPY_ts, k =-1)*100)
# Since 0.07 lies inside 0.463, we accept H0, series is stationary. Percentage change should be used.
SPY_ts_percent_adjusted <- diff(SPY_ts)/stats::lag(SPY_ts, k =-1)*100

# Get the length of ts object
length(SPY_ts_percent_adjusted)

# Splitting train test set. Split the data into 80% training set, and 20% test set. 

SPY_train <- window(SPY_ts_percent_adjusted, start = c(2004,1), end = c(2019,12))

SPY_test <- window(SPY_ts_percent_adjusted, start = c(2020,1), end = c(2024,1))

# Since there are 2 major events that caused the drop in prices, covid-19 and 
# global financial crisis in 2008, we have both events taken into account in both the sets.

## Classical decomposition ====================================================================
# Decompose and get the seasonally adjusted part and seasonal part
seasadj_SPY <- seasadj(decompose(SPY_train))

seasonal_SPY <- seasonal(decompose(SPY_train))

# How many diffs are needed?
nsdiffs(seasadj_SPY)
ndiffs(seasadj_SPY)
# seasadj: 0 for both nsdiffs and diffs, reaffirms our point that percent change data is stationary

# Define a fxn that runs benchmark models on seasadj data and gets accuracy score
# based on test score
get_test_results <- function(benchmark_model){
  model_forecast <- forecast(benchmark_model)
  accuracy(model_forecast, SPY_test)
}

# Seasonal
snaive_seasonal_SPY <- snaive(seasonal_SPY, h=length(SPY_test))
get_test_results(snaive_seasonal_SPY)

# Naive
get_test_results(naive(seasadj_SPY, h=length(SPY_test)))

# Seasonal naive
get_test_results(snaive(seasadj_SPY, h=length(SPY_test)))

# Drift
get_test_results(rwf(seasadj_SPY, h=length(SPY_test)))

# Mean
get_test_results(meanf(seasadj_SPY, h=length(SPY_test)))
# Mean method is best as it has lowest rmse and mape

meanf_SPY <- meanf(seasadj_SPY)

# Examine residuals of our preferred method 
checkresiduals(meanf_SPY)
# Small p value meaning that there are still time series info (autocorrelations) in the model

checkresiduals(meanf_SPY$residuals + snaive_seasonal_SPY$residuals)
# Small p value meaning that there are still time series info (autocorrelations) in the model

## MSTL decomposition===================================================================
seasadj_SPY <- seasadj(mstl(SPY_train))

seasonal_SPY <- seasonal(mstl(SPY_train))

# How many diffs are needed?
nsdiffs(seasadj_SPY)
ndiffs(seasadj_SPY)
# seasadj: 0 for both nsdiffs and diffs, reaffirms our point that percent change data is stationary

# Seasonal
snaive_seasonal_SPY <- snaive(seasonal_SPY, h=length(SPY_test))
get_test_results(snaive_seasonal_SPY)

# Naive
get_test_results(naive(seasadj_SPY, h=length(SPY_test)))

# Seasonal naive
get_test_results(snaive(seasadj_SPY, h=length(SPY_test)))

# Drift
get_test_results(rwf(seasadj_SPY, h=length(SPY_test)))

# Mean
get_test_results(meanf(seasadj_SPY, h=length(SPY_test)))
# Mean method is best as it has lowest rmse and mape

meanf_SPY <- meanf(seasadj_SPY)

# Examine residuals of our preferred method 
checkresiduals(meanf_SPY)
# fails the ljung box test

checkresiduals(meanf_SPY$residuals + snaive_seasonal_SPY$residuals)
# fails the ljung box test.. time to give up :,)
# with tiny improvements over classical decomposition


# Q3 =================================================================================
plastics

autoplot(plastics)
acf(plastics)
# There is seasonality - data peaks at associated lags suggestive of seasonality
# and trend - slowly decaying lags

decompose(plastics, type = c("multiplicative")) |>
  autoplot()
# Yes, the results support interpretations from a. Trend is upward sloping and
# there is seasonality.

seasonally_adjusted <- seasadj(decompose(plastics, type = c("multiplicative"))) 
seasonally_adjusted |>
  autoplot()

# adding 500 to the second last observation of plastics, to act as outlier
plastics[59] <- plastics[59]+500
plastics

seasonally_adjusted2 <- seasadj(decompose(plastics, type = c("multiplicative")))
seasonally_adjusted2 |> 
  autoplot()

rm(plastics)

# Adding 500 to data in the middle to act as outlier
plastics[30] <- plastics[30]+500
seasonally_adjusted3 <- seasadj(decompose(plastics, type = c("multiplicative")))
seasonally_adjusted3 |> 
  autoplot()

rm(plastics)

# imo, there is no diff to outlier be it last few or middle observation.

# Q4 ==============================================================================
autoplot(bricksq)

decompose(bricksq)|>
  autoplot()

# wth is trendcycle and seasonal indices?

seasonal_brick_ts <- seasadj(decompose(bricksq))

autoplot(seasonal_brick_ts)

naive(seasonal_brick_ts)

# stlf forecasting for original data
stlf(bricksq)|>
  autoplot()
checkresiduals(stlf(bricksq))
# Residuals are correlated

# Robust stlf
stlf(bricksq, robust=TRUE)
checkresiduals((stlf(bricksq, robust=TRUE)))
# p value improved a lot (way higher) but still <0.05 hence residuals are still
# correlated

# Set test window as last 2 years and training window as preceding period
train <- window(bricksq, end=c(1992,4))

test <- window(bricksq, start=c(1993,1), end=c(1994,4))

# Forecasting onto the test set (last 2 years or equivalent to 7 quarters as 1994 Q4
# data is not present)
snaive_forecast <- forecast(snaive(train), h=7)
stlf_forecast <- forecast(stlf(train),h=7)

# Comparing test scores
accuracy(snaive_forecast, test)
accuracy(stlf_forecast, test)

# stlf has lower out of sample test MAPE and RMSE, hence it performs better than snaive.

