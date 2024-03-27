# Green data test script==========================================================

# Green data consists of temp, Co2 and sea levels. 
# Models to be ran : VAR (only of lag 1 to prevent overfitting), ARIMA-X and
# VAR-X (where the exogenous variable is sea levels)

library(tidyverse)
library(fpp2)
library(urca)

# Load data
green_df <- read_csv("C:/Users/rayne/Documents/GitHub/DSA301_TimeSeries/Data/green_data_pc_new.csv")|>

green_ts <- ts(green_df[,3:5], start=)
