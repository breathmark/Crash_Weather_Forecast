# Crash_Weather_Forecast
### Date: July, 2023, Modified July 30, 2024
### Project Title: Using weather features in two linear regression models to predict the accident totals and severity of motor vehicle crashes in the Washington D.C. city
### Dataset: 
  #### Crashes in DC, from Open Data DC from 2011 to 2022
  (https://opendata.dc.gov/datasets/DCGIS::crashes-in-dc/explore?showTable=true)
  #### Washington, DC Weather Data, from the National Centers for Environmental Information
  (https://www.ncdc.noaa.gov/)
  #### Software: R
### Project Summary:
This project joins vehicle crash data set with an independent weather dataset on Date variable. 
Significant feature engineering is required to structure a proper dataset for regression analysis:
  1. aggregation of high-grain hourly crash data to daily crash data;
  2. high dimensionality reduction based on data set structure;
  3. creation of two dependent variables of Accident_Total (i.e. daily total of crash accidents) and Severe_Perc (i.e. daily percentages of severe accidents) for analysis purpose;
  4. creation of two crash features IsImpaired and IsSpeeding
  5. time-series crash data histograms against Year, Month, WeekDay, Hour that identified clerical data entry anomalies for data prior 2016
  6. Selected the period from 2016 to 2022 with covid period identifier (IsCovid for years 2020 - 2022)
  7. weather feature selections (snow, rain, wind, minimum tempreture)
  8. creation of additional weather features IsSnow, IsFreezing, IsRain
  9. Linear relationship with dependent variables, multi-collinearity, distribution characteristics, and data imbalance in the weather variables are thoroughly examined
Model Buidling uses an iterative additive process, Stepwise process and 10-fold cross validation. MSE, MAE, residual plot, and QQ plot on the fit and predict models are compared and examined
### Conclusions:
  1. Hazardous weather data is a highly imbalanced data set. From 2016 to 2022, 2% of dates have Snow, 11% of dates have freezing minimum temprature, and 34% of dates have raining conditions
  2. Crash data has imbalanced features: 2.89% of crashes have impaired parties involved, and 1.1% of crashes have speeding
  3. The imbalanced data contributes to low R-square statistics (0.02 ~ 0.03)  for the models using only weather features, and only crash features respectively
  4. Weather features have non-linear relationships with crash data, thus violating one of the assumptions of linear regression models
  5. Month and WeekDay have more predicting power on crash data with R-square being 0.12
  6. Best models use predictors: Snow, Minimum Temprature, IsImpaired, Month, Weekday, MSE 219, R-square 0.16
  7. Test models: MSE 498, MAE 53%, R-square 0.11
  8. Odd but interesting insight: Snow has negative coefficient with crash data. The more snow, less traffic, less crashes?


