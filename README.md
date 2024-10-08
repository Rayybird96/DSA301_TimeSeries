# DSA301 (Time Series Analytics)
This repository contains my __homework, project, and learning process__ for SMU module DSA301 (Time Series Analytics).

# Project description
__Climate change, Human-caused or not?__

![image](https://github.com/user-attachments/assets/9c58025b-83a5-4f57-8523-571e28365141)


Long-term climate change research has been modelled as an interaction between the following 3 variables: 
1. Greenhouse gas emissions
2. Global average temperatures
3. Sea levels
   
Other indicators (either causal factors or outcomes) have been proposed, such as deforestation, droughts & species extinction. 

In our project, we explore the effects of anthropogenic activities on climate change, determine if there are robust relationships between potential explanatory variables using explanatory time series models (eg. *VAR, ARIMA-X, ARMA-X*), and consider/test alternative explanations.

## Methodology

![image](https://github.com/Rayybird96/DSA301_TimeSeries/assets/138758608/975b2342-eafa-403e-84ca-488ed7482b16)

We will be building a green model (green variables) and a yellow model (yellow + green variables). The green model consists of __natural variables (green)__, while the yellow model includes __independant human-caused variables (yellow)__.

We will be using basic *autoregressive intergrated moving average (ARIMA)* for both models, to act as a benchmark against more complex models.
The green variables interact with one another, hence can be modelled with *vectorised auto regression (VAR)* and *vector error correction model (VECM)*.
The yellow variables are independant, hence the yellow model can be modelled by *VAR-X, VECM-X and ARIMA-X*.

Disclaimer on *VAR* and *VECM* methodology: I wanna acknowledge that our *VAR* and *VECM* (extension of *VAR*) was not entirely applied correctly as we did not diff, nor BoxCox transform, the series beforehand, and applied it on non-stationary data. Recall that series has to be covariance stationary - constant mean, constant variance, before modelling on to ensure that estimated parameters will be constant over time! :(

However, further limitations of *VAR & VECM* such as ease of overfitting (as coefficients scale very quickly!) and cointegration/granger casuality tests have all been accounted for :)

Our concluding models are *ARIMA* favoured, due to these models having less of an overfitting problem. Yearly data does not present enough data for *VAR* and *VECM* models, which scales in coefficients very quickly. Moreover, we must consider the train test split and forecast length as well, all of which requires many years of data. This is further pertinent when we consider that, as a Machine Learning rule of thumb, each coefficient should have at least 10 observations (10 years). For the math behind this and more, please refer to the project pdf!

# R Documentation
ts_proj is the main project file, ts_hw are my homework files, and time_series_learnings contains my learnings up till week 6. 

Final updates: I also added my final examination script and pdf files although there is no answer key and I have no idea how well (or badly) I did.

