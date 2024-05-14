#ASDV R file#: 3  
#Contains the R code for Implementation of step 4.5 of Task 1 of ASDV Assignment 
#==========================================================================
# Main Objective: A Comparative Exploratory Analysis of the Employment 
# factors influencing the GDP per person Employed between
# 3 income groups of Euro-African countries in 2011-2020
#==========================================================================
#Objective of this R Code:
#Aim to forecast GDP-per-worker$ for Low-income countries using both 
# Holt-Winters and ARIMA models and evaluate and assess the best out of them.  
# The consistent GDP growth observed in low-income countries has captured the 
# attention of researchers, making it a focal point for this experiment.
#==========================================================================
#setting the working directory#
#setwd("C:/ASDV/ASDV_Activity")
# installing Useful statistical packages for performing Time Series Analysis#######
#Installing required packages for Time Series

install.packages("TTR") 
install.packages("forecast") 
install.packages("dplyr")
library(TTR) 
library(forecast)
library(dplyr)
#SMA() function in the “TTR” R package
#“forecast()” function and auto.arima() functions are parts of the forecast package in R.
#(eg.:forecast.HoltWinters() function)
#However, acf() and pacf() are part of the base R installation
# acf() stands for AutoCorrelation Function and pacf() stands for Partial AutoCorrelation Function
#The box.test() function in R belongs to the stats package, its a default package in R library(stats)

options(scipen=999) 
#4.5 Forecasting using Time series using HOLT winter and ARIMA and finding the Best Suitable Model########
#Loading already pre-processed dataset
emp_gdp_df = read.csv("employment_on_gdp_new.csv")
#Preliminary Overview of the Dataset
head(emp_gdp_df)
dim(emp_gdp_df)
#[1] 120  10
#Validating Data by Checking the presence of Missing values#####
colSums(is.na(emp_gdp_df))

#Columns: X,Year, Country,GDP_PP_EMP,EMP_Agriculture,EMP_Service,EMP_Ser_F,EMP_Ser_M,EMP_Industry,Income_Grp
#Dropping not required 'X' column 
emp_gdp_df1 <- subset(emp_gdp_df, select = -c(X))
#Overview of the Dataset and Verification of Null Values
dim(emp_gdp_df1)  #Shape of dataset
str(emp_gdp_df)

# Preparing time Series Data##### 
# Loading the Subset data of Low-income countries Only. 4 countries for 10 years. 4*10 =40 instances
Low_income_gdp <- emp_gdp_df1[emp_gdp_df1$Income_Grp == "Low", ]
Low_income_gdp
dim(Low_income_gdp)
# Calculating the Average (Mean) of GDP per person employed of all 4 low income countries into one 
#that is grouped by year to have a frequency of one reading(or observation) per year
mean_gdp_by_year <- Low_income_gdp %>%
  group_by(Year) %>%
  summarise(LI_GDP_timeseries = mean(GDP_PP_EMP))
mean_gdp_by_year
mean_gdp_by_year['LI_GDP_timeseries']
LI_gdp_ts<- ts(mean_gdp_by_year['LI_GDP_timeseries'], frequency=1, start=c(2011))
#Listing the Time Series GDP data for Low income countries
LI_gdp_ts

#Plotting the Time series data using plot.ts() function
plot.ts(LI_gdp_ts)

#Decomposing Time Series using SMA() for clear View on the Trend of the GDP-per-workr$####
#Estimating the trend component of GDP time series by 
#smoothing using a simple moving average using SMA() function
# of order 3, and plot the smoothed time series data,
LI_gdp_ts_SMA3 <- SMA(LI_gdp_ts, n=3)
#Plotting the trend of Average GDP-per-workr$ values of low income countries
plot.ts(LI_gdp_ts_SMA3)
#Significantly constant growth could be seen on the GDP_PP_EMP value of 
#the Low Income group countries
#Since the observation frequency is one per year and not a seasonal based data so its not possible 
#to decompose and show the Components such as trend, seasonal, and irregular components of the time series
#LI_gdp_tscomponents <- decompose(LI_gdp_ts)

#Part1:Forecasting on Time Series Data using Holt-Winters Exponential Smoothing Model#####
#Holt-Winters Exponential Smoothing (additive model with no trend and no seasonality)
# Since the dataset is an additive model with constant level and no seasonality simple exponential 
# smoothing is sufficient  to perform short-term forecasts
#Thus, forecast could be computed using simple exponential smoothing method. by setting 
# the parameters beta=FALSE and gamma=FALSE in the HoltWinters() function
#step1.1 Forecasts for the same Period existing Timeseries in the dataset#######
LI_gdp_tsforecast <- HoltWinters(LI_gdp_ts, beta=FALSE, gamma=FALSE)
LI_gdp_tsforecast
#The alpha value is alpha: 0.9999286. But has high coefficients value of 5228.579
#By default, HoltWinters() makes forecasts for the same time period covered by the original time series dataset.
#The forecasts made by HoltWinters() are stored in list variable named “fitted” 
LI_gdp_tsforecast$fitted
#Almost all the Predicted values as same as the original time period values
#Which could be visually proved through below plot() command

#We can plot the original time series against the forecasts 
plot(LI_gdp_tsforecast)

#Findings: In this above plot, the original time series shown in Black line and the forecasts as a Red line. 
#It could be noted that forecasted time series is much smoother than the time series of the original data  

#step1.2: Measure of the accuracy of the forecasts####
#As a measure of the accuracy of the forecasts: are the Sum-of-squared-errors. 
#This is stored in the named list variable, “SSE”
LI_gdp_tsforecast$SSE
# [1] 112385.5

#step1.3: Forecasting next 10 years Average of GDP per person employed#######
#Now, forecast() method is used to forecast Average GDP per person employed(GDP-per-worker$) value of 
#Low income countries for next 10 yeas, specified using h argument(h=10)
LI_gdp_tsforecast_new <- forecast(LI_gdp_tsforecast, h=10)
#Listing the New forcasted timeseries data
LI_gdp_tsforecast_new

#Below will plot the forecasted time series of the Low income countries
plot(LI_gdp_tsforecast_new)
#In the forecast plot, the forecast of 2021 to 2030 are plotted as a blue line, 
#In whcih the 80%  prediction interval is portrayed as purple shaded area, 
#and the 95% prediction interval is portrayed as gray shaded area.

#Actually, errors of Forecast usually said as Forecast errors are stored in the named element “residuals”
#If the predictive model cannot be improved upon, there should be no correlations 
#between forecast errors for successive  predictions

#1.4 Assessing Autocorrelation on Residuals#########
#Plotting Correlogram on the residuals(forecast errors) to Assess whether the 
#Residuals are the independent and normally distributed 
#for lags 1-20 using acf() function 
acf(LI_gdp_tsforecast_new$residuals, lag.max=20 , na.action = na.pass)

#The above plot gives a suitable to view on the sample correlogram to ensure whether the 
#autocorrelation is not touching the bounds. 
#The same could also be counter verify by performing  Ljung-Box test using Box.test() method.

Box.test(LI_gdp_tsforecast_new$residuals, lag=4, type="Ljung-Box")

# Ljung-Box test statistic is 1.4842, and the p-value is p-value = 0.8294(>0.05),
# pvalue more than 0.05. Thus, we should not reject Null Hypothesis.
#Hence, There is no correlation. Meaning that they are independent
# so there is evidence of non-zero auto correlations in the in-sample forecast errors at lags 1-4.
#Since there is no correlation, now  check whether the forecast errors are normally distributed 
# with mean zero and constant variance.

#step1.5: Assessing Normality on Residuals#########
plot.ts(LI_gdp_tsforecast_new$residuals)
# The plot shows that the in-sample forecast errors seem to have 
# smooth variance with 2 notable fluctuations at 2014 and 2019
#over the time period. The former is higher than the later one.


#step1.6: Visually assessing the Normality by plotting histogram#####
#library("forecast")
#important function named forecasterrors to plot histogram

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#Removing Null values
LI_gdp_tsforecast_new$residuals <-LI_gdp_tsforecast_new$residuals[!is.na(LI_gdp_tsforecast_new$residuals)]
#FIRST Visualization for Proof of Normality of Forecast errors/ Residuals
plotForecastErrors(LI_gdp_tsforecast_new$residuals)
#Plot depicts the Residuals of HoltWinter forecast Model are not normally distributed around the Mean Zero

FCresiduals = LI_gdp_tsforecast_new$residuals
#From the above plot, its is understood that though there is no correlation and normal 
#however the forecast errors are not distributed with mean zero and constant variance
#Hence, used the usual method as coded below to display the data distribution with A kurtosis line
#SECOND Visualization for Proof of Normality of Forecast errors####
hist(FCresiduals, main = "Histogram of Forecast Errors ", 
     xlab="ForecastErrors",
     col = "red", border = "black", probability = TRUE)
# Here, probability = TRUE to see as density. We can exclude this to see frequency 
# Overlaying a Density plot on the histogram
lines(density(FCresiduals), col = "blue", lty = 2, lwd = 3)
# Though the forecast errors are not centered around the zero, more or less normally 
# distributed with a slight skew in the left compared to a normal curve. 

#Sharipo Test for Normality of Forecast errors(Residuals)######
#Further, did the shapiro test, to confirm the normality of the residuals
shapiro.test(FCresiduals)
# The test result a p-value= 0.9115. So, we should not reject the Null hypothesis and reject the 
#alternative hyposthesis. Therefore, the Residuals(forecast errors) are Normally distributed

#Part2: Forecasting using ARIMA Model#####
#Objective: To perform forecasting on the given time series using ARIMA Model#
#step 2.1 Differencing a Time Series to make it stationary #######
#Since, ARIMA models are defined for stationary time series therefore need to ‘difference’ the 
#time series until we obtain a stationary time series.
#Performing First differencing#####
LI_gdp_ts
LI_gdp_tsdiff1 <- diff(LI_gdp_ts, differences=1)
plot.ts(LI_gdp_tsdiff1)
#resulting time series of first differences (above) does not appear to be stationary in mean. 
#Therefore, its needed to difference the time series twice,
#Performing Second differencing ##########
LI_gdp_tsdiff2 <- diff(LI_gdp_ts, differences=2)
plot.ts(LI_gdp_tsdiff2)
# The time series of second differences (above) appears to be stationary in mean and variance, as the level 
# of the series stays roughly constant over the time, and the variance of the series appears roughly constant 
# over time. Thus, its is required to  difference the time series for two times on the Average GDP per 
# person Employed(of Low Income coutries) in order to achieve a stationary series.

#Its a thumrule that Whenever a time series is differenced to make it stationary we must use the 
#ARIMA(p,d,q) model for the  time series, here 'd' is the differencing order.
#Thus, for the GDP timeseries ARIMA(p,2,q) model can be used.

#Step2.2: Performing forecast on the stationary time series using ARIMA Model ##########
#On a stationary or transformed as a stationary timeseries which is differenced d times, 
#we must select the appropriate values of p and q for an ARIMA(p,d,q) model.
#Its is required to examine the correlogram and partial correlogram of the stationary time series
#To find p and q values. “acf()” function is used to plot a correlogram and “pacf()” function is to plot 
# partial correlogram in R.Thus, ACF and PACF plots helps to identify the potential ARIMA(p, d, q) 
# parameters for your time series model. Here, d =2

# Plotting a correlogram using acf()
acf(LI_gdp_tsdiff2, lag.max=20) 

#step2.2.1 get the actual autocorrelation values set plot=FALSE to find q value####
acf(LI_gdp_tsdiff2, lag.max=20, plot=FALSE) 
#From the correlogram, There are no significant spikes or does not exceed beyond the lag  of 1, 
#suggesting that there might not be strong auto correlations in the series.
 
# Plotting a partial correlogram
pacf(LI_gdp_tsdiff2, lag.max=20)
# PACF plot also shows no significant spikes beyond lag of 1, 
# indicating that the partial autocorrelations at higher lags are not significant

#Computing partial autocorrelations to find p value
pacf(LI_gdp_tsdiff2, lag.max=20, plot=FALSE) 
# Interpretation of pacf() results:
# The PACF values you provided suggest that the partial autocorrelation at lag 5 is relatively high. 
# This could be an indication that an  ARIMA(0,2,5) model could be worth exploring.


#step 2.2.3: Fitting ARIMA(0,2,5) model#########
gdp_ts_arima_model <- arima(LI_gdp_ts, order = c(0, 2, 5))

# Summary of the model
summary(gdp_ts_arima_model)

#Here, since q=5 so 5 estimated coefficients for the moving average (MA)
#terms are given as ma1 to ma5. When we are fitting an ARIMA(0,2,5) model to the time
# series, it means we are fitting an an ARMA(0,2) model to the time series of second differences. 
#An ARMA(0,21) model can be written X_t - mu = Z_t - (theta * Z_t-2), where theta is a parameter
# to be estimated. From the output of the “arima()” R function (above), the estimated value of 
#theta (given as ‘ma5’ in the R output) is 0.8020 in the case of the ARIMA(0,2,5) model fitted to the
# time series of GDP per person Employed.

#Steps2.2.4: Forecasting Using an ARIMA Model#########
gdp_ts_arima_forecasts <- forecast(gdp_ts_arima_model, h=10)
#Listing the forecast for 10 years
gdp_ts_arima_forecasts
 
#Plotting the Forecast
plot(gdp_ts_arima_forecasts)

#step2.2.5: Assessing Autocorrelation on ARIMA model's Residuals(Forecast-errors)#####
acf(gdp_ts_arima_forecasts$residuals, lag.max=20)
Box.test(gdp_ts_arima_forecasts$residuals, lag=4, type="Ljung-Box")
# p-value = 0.9342 is above 0.05 hence we must not reject the Null hypothesis that 
#there is not autocorrelation
plot.ts(gdp_ts_arima_forecasts$residuals)

#Step2.2.6: Assessing Normality on ARIMA model's Residuals(Forecast-errors)####
FC_Arima_residuals= gdp_ts_arima_forecasts$residuals
# Plotting histogram to  Assessing Normality 
plotForecastErrors(gdp_ts_arima_forecasts$residuals) 

#From the above plot it is very obvious that the forecast errors are distributed with mean zero and 
#with constant variance Which did not happened in Holt winter model. 
#Conclusion: Thus, by difference twice on the GDP-per-worker values of Low-income countries 
# forecast will be better with ARIMA Model since it gives a better normally distributed residuals 
#than the Holt winter

#Comparing Mean and Accuracy of Holt winter & ARIMA models#######
# Computing Mean and Accuracy on Residuals of both Models  #####
#Mean of Residuals of Holt Winters forecast####
mean(FCresiduals)
#1] 108.4345
#Mean of Residuals of ARIMA forecast
mean(FC_Arima_residuals)
#[1] 4.25408
#Accuracy of Forecast using Holt Winter
accuracy(LI_gdp_tsforecast_new)
#Accuracy of Forecast using Arima
accuracy(gdp_ts_arima_forecasts)
# Interpretation and Conclusion:
# 1. Holt-Winters Model:
# The ACF1 value close to 0 indicates little remaining autocorrelation in the residuals.
# 2. ARIMA Model:
# The MPE and MAPE values are low, indicating accurate percentage forecasts.
# The MASE value less than 1 indicates that the ARIMA model is performing better than a naïve model.
# The ACF1 value close to 0 suggests little remaining autocorrelation in the residuals.
# Conclusion:
# Both models seem to provide reasonably accurate forecasts, but the ARIMA model appears to have 
# lower average forecast errors compared to the Holt-Winters model.


#Ref for proof of literaure that ARIMA is best suited model to do forecast on
#GDPs https://mpra.ub.uni-muenchen.de/108912/1/MPRA_paper_108912.pdf

#Timeseries Ends here ##########