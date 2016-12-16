# Time Series: Canadian Bankruptcy Rates Forecasting
**Key Word: Time Series Modeling(box-jenkins,exponential smoothing,vector autoregression), R**


##Introduction

This project presents the analysis and construction of SARIMA,HotlWinter,ARIMAX,VAR time series models to accurately forecast monthly national bankruptcy rates in Canada.

The models are composed and validated for the period from  January 1987 to December 2010. Composed models are then used to forecast bankruptcy rate in the years of 2011-2012.

##Data
The dataset consists of 4 series of monthly data from January 1987 to December 2010:

* Bankruptcy rate
* Population
* Unemployment rate
* House Price Index

![alt tag](https://github.com/qianmx/TimeSeries-Canadian-National-Bankruptcy-Rate/blob/master/plots/data1.png)
![alt tag](https://github.com/qianmx/TimeSeries-Canadian-National-Bankruptcy-Rate/blob/master/plots/data2.png)

##Model
* The detailed model builing process can be find in Report.pdf in this repository.
* The R scripts can be found in R_code repository.

##Forecast
![alt tag](https://github.com/qianmx/TimeSeries-Canadian-National-Bankruptcy-Rate/blob/master/plots/forecast.png)
