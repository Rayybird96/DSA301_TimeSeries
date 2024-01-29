library(tidyverse)
library(fpp2)

#Q1 ===============================================================================
# Data cleaning 
nat_gas_df <- read_csv("NG_CONS_SUM_DCU_NUS_M.csv")
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

# get trend component
t <- trendcycle(decompose(nat_gas_ts))

t|> 
  autoplot()

# how many order of differencing needed?
nsdiffs(t) #0

ndiffs(t) #1

# 1 order diff of t
t_diff <- diff(t)
t_diff|>
  autoplot()

# autoplot(t_diff)
# autoplot(t_diff) + autolayer(meanf(t_diff, h = 11))
# autoplot(t_diff) + autolayer(naive(t_diff, h = 11))
# autoplot(t_diff) + autolayer(snaive(t_diff, h = 11)) 

# Seasonal component
s <- seasonal(decompose(nat_gas_ts))

# Take a look
autoplot(s)

# Ljung box test
checkresiduals(snaive(s))

# snaive model on s
s_model <- snaive(s, h=12)

# naive model on t
t_model <- naive(t_diff, h = 12)

print(s_model)
print(t_model)

model_df <- (print(s_model)+print(t_model))
model_df$`Point Forecast`

# converting to ts
as.ts(model_df$`Point Forecast`, deltat=1/12)
accuracy(as.ts(model_df$`Point Forecast`, deltat=1/12))
checkresiduals(as.ts(model_df$`Point Forecast`, deltat=1/12))

# p-value of 0.1019>0.05, we accept H0 that there's no autocorrelation


#Q2 ===============================================================================
spy_df <- read_csv("SPYmonthly2003.csv")

SPY_df <-  read_csv("SPYmonthly2003.csv") |> select(Date, Open) #columns Date and Open selected
SPY_ts <- ts(SPY_df["Open"],start= c(2004,01), end = c(2024, 01), deltat = 1/12)

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
# since 0.07 lies inside 0.463, we accept H0, hence series is stationary. Percentage change should be used.
SPY_ts_percent_adjusted <- diff(SPY_ts)/stats::lag(SPY_ts, k =-1)*100

# Get the length of ts object
length(SPY_ts_percent_adjusted)

# Splitting train test set. Split the data into 80% training set, and 20% test set. 
# About 4 years as test set and 16 years as training set.

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
nsdiffs(seasonal_SPY)
# seasadj: 0 for both nsdiffs and diffs, reaffirms our point that percent change data is stationary
# seasonal: 1 order seasonal differencing needed

seasonal_SPY <- diff(seasonal_SPY, 12)

# Define a fxn that runs benchmark models on seasadj data and gets accuracy score
# based on test score
get_test_results <- function(benchmark_model){
  model_forecast <- forecast(benchmark_model)
  accuracy(model_forecast, test)
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

## MSTL decomposition==================================================================='
seasadj_SPY <- seasadj(mstl(SPY_train))

seasonal_SPY <- seasonal(mstl(SPY_train))

# How many diffs are needed?
nsdiffs(seasadj_SPY)
ndiffs(seasadj_SPY)
nsdiffs(seasonal_SPY)
# seasadj: 0 for both nsdiffs and diffs, reaffirms our point that percent change data is stationary
# seasonal: 1 order seasonal differencing needed

seasonal_SPY <- diff(seasonal_SPY, 12)

# Define a fxn that runs benchmark models on seasadj data and gets accuracy score
# based on test score
get_test_results <- function(benchmark_model){
  model_forecast <- forecast(benchmark_model)
  accuracy(model_forecast, test)
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

# Q5=================================================================================
plastics

autoplot(plastics)
acf(plastics)
# there is seasonality - data peaks at associated lags suggestive of seasonality
# and trend - slowly decaying lags

decompose(plastics, type = c("multiplicative")) |>
  autoplot()
# yes, the results support interpretations from a. Trend is upward sloping and
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

# adding 500 to data in the middle to act as outlier
plastics[30] <- plastics[30]+500
seasonally_adjusted3 <- seasadj(decompose(plastics, type = c("multiplicative")))
seasonally_adjusted3 |> 
  autoplot()

rm(plastics)

# imo, there is no diff to outlier be it last few or middle observation.

# Q6 ==============================================================================
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
# residuals are correlated

stlf(bricksq, robust=TRUE)
checkresiduals((stlf(bricksq, robust=TRUE)))
# p value improved a lot (way higher) but still <0.05 hence residuals are still
# correlated

train <- window(bricksq, end=c(1992,4))

test <- window(bricksq, start=c(1993,1), end=c(1994,4))

snaive_forecast <- forecast(snaive(train), h=7)
stlf_forecast <- forecast(stlf(train),h=7)

accuracy(snaive_forecast, test)
accuracy(stlf_forecast, test)


