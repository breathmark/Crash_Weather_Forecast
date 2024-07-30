# Crash_Weather_Forecast
### Date: July, 2023, Modified July 30, 2024
### Project Title: Using weather features in two linear regression models to predict the volume and severity of motor vehicle crashes in the Washington D.C. city
### Dataset: 
  #### Crashes in DC, from Open Data DC 
  (https://opendata.dc.gov/datasets/DCGIS::crashes-in-dc/explore?showTable=true)
  #### Washington, DC Weather Data, from the National Centers for Environmental Information
  (https://www.ncdc.noaa.gov/)
  #### Software: R
### Project Summary:
##### This project joins vehicle crash data set with an independent weather dataset on Date variable. 
Significant feature engineering is required to structure a proper dataset for regression analysis:
  1. aggregation of high-grain hourly crash data to daily crash data;
  2. high dimensionality reduction based on data set structure;
  3. creation of two dependent variables of Accident_Total (i.e. daily crash volumns) and Severe_Perc (i.e. daily percentages of severe accidents) for analysis purpose;
  4. creation of two crash features IsImpaired and IsSpeeding
  5. time-series histograms against Year, Month, WeekDay, Hour to discover distribution and identify clerical data entry anomalies
  6. weather feature selections (snow, rain, wind, minimum tempreture)
  7. creation of additional weather features IsSnow, IsFreezing, IsRain
  8. Linear relationship with dependent variables, multi-collinearity, distribution characteristics, and data imbalance in the weather variables are thoroughly examined
Model Buidling uses an iterative additive process, Stepwise process and 10-fold cross validation. MSE, MAE, residual plot, and QQ plot on the fit and predict models are compared and examined.
Key observations:
  1. TBA


