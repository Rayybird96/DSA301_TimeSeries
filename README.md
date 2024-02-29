# DSA301 (Time Series Analytics)
This repository contains my __homework, project, and learning process__ for SMU module DSA301 (Time Series Analytics).

# Project description
__Climate change, Human-caused or not?__

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
The yellow variables are independant, hence the yellow model can be modelled by *VAR-X, VECM-X and ARIMA-x* (where yellow variables are the exogenous variables).

# R Documentation
ts_proj is the main project file, ts_hw are my homework files, and time_series_learnings contains my learnings up till week 6.

