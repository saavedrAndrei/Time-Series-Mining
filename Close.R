#Analysis of Tractor Series Data
#Credit to YOU CANalytics; Step-by-Step Graphic Guide to Forecasting through ARIMA Modeling using R - Manufacturing Case Study Example (Part 4)
# Roopam Upadhyay

#rm(list=ls())

library(forecast)
library(tseries)
library(ggplot2)

#Read Data
# data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
data = read.csv('all_stocks_5yr.csv')

Name <- "RJF"
df <- subset(data, Name == "RJF")
df
df = ts(df[,5],start = c(2013,2),frequency = 365)
df
df <- na.omit(df)

## Original Dataset
#Clearly the above chart has an upward trend for tractors sales (and there is also a seasonal component).
x11()
plot(df, xlab='date', ylab = 'close')

x11()
par(mfrow = c(2,1))
acf(ts(df),main='ACF RJF-Close')
pacf(ts(df),main='PACF RJF-Close')


## LOG Dataset - No Effect
#Transform data to make data stationary on variance
x11()
plot(log10(df),ylab='Log close')

x11()
par(mfrow = c(2,1))
acf(ts(log10(df)),main='ACF RJF-Close')
pacf(ts(log10(df)),main='PACF RJF-Close')

## DIFF - LOG
#Difference log transformed data to make data stationary on both mean and variance
x11()
plot(diff(log10(df)),ylab='Log Diff RFJ-Close')

x11()
par(mfrow = c(2,1))
acf(ts(diff(log10(df))),main='ACF RJF-Close')
pacf(ts(diff(log10(df))),main='PACF RJF-Close')


fit_test_01<- arima(df, order = c(0,1,1), include.mean=TRUE, method=(c("ML")))
fit_test_01

coef(fit_test_01)
summary(fit_test_01)

fit_test_02<- arima(df, order = c(1,0,1), include.mean=TRUE, method=(c("ML")))
fit_test_02

coef(fit_test_02)
summary(fit_test_02)


fit_test_03<- arima((df), order = c(0,1,0), include.mean=TRUE, method=(c("ML")))
fit_test_03

coef(fit_test_03)
summary(fit_test_03)

## NUEVO

#stationary? log(AirPassengers); Seasonal Difference diff(logap,lag=12)? Regular differences?
logap<-log(df)
plot(logap, xlab='date', ylab = 'open')

test_01 = arima(df,  order=c(0,0,1), seasonal=list(order = c(0, 0, 1), period=1))
coef(test_01)
summary(test_01)

test_02 = arima(df,  order=c(0,0,0), seasonal=list(order = c(0, 1, 0), period=1))
coef(test_02)
summary(test_02)

test_03 = arima(df,  order=c(0,1,0), seasonal=list(order = c(0, 0, 0), period=1))
coef(test_03)
summary(test_03)

#Forecast
fcastmod1MA1MAS <- forecast(fit_test_03, h=365)
X11()
plot(fcastmod1MA1MAS)


x`#Residuals of the models
#residmod1MA1MAS<-residuals(mod2MA)s
X11()
par(mfrow=c(2,1))
acf(fit_test_03)
pacf(fit_test_03)


#diagnosis modelo
X11()
diagnosismod1MA1MAS<-checkresiduals(test_03)


# Additional tasks:
# - Obtain residuals of other models, and plot the ACF and PACF of the residuals. Check if they follow a "white noise" distribution
# - histogram of the residuals and check normal distribution (Kolmogorov-smirnov test, normal probability plot)
# - compare models. 

#Since, there are enough spikes in the plots outside the insignificant zone (dotted horizontal lines) 
#we can conclude that the residuals are not random. This implies that there is juice or information 
#available in residuals to be extracted by AR and MA models. Also, there is a seasonal component available 
#in the residuals at the lag 12 (represented by spikes at lag 12). This makes sense since we are analyzing 
#monthly data  that tends to have seasonality of 12 months because of patterns in tractor sales.

# Identification of best fit ARIMA model
require(forecast)
ARIMAfit = auto.arima((log10(df)), approximation=FALSE,trace=TRUE)
summary(ARIMAfit)
#The best fit model is selected based on Akaike Information Criterion (AIC), and 
#Bayesian Information Criterion (BIC) values. The idea is to choose a model with minimum AIC and BIC values. 
#The values of AIC and BIC for our best fit model developed in R are displayed at the bottom of the  results

#As expected, the model has I (or integrated) component equal to 1. This represents differencing of order 1. 
#There is additional differencing of lag 12 in the above best fit model. 
#Moreover, the best fit model has MA value of order 1. Also, there is seasonal MA with lag 12 of order 1.


#The next step is to predict tractor sales for next 3 years i.e. for 2015, 2016, and 2017 through 
#the above model. The following R code does this job for us.


#Forecasted values of tractor sales in blue. 
#Also, the range of expected error (i.e. 2 times standard deviation) is displayed 
#with orange lines on either side of predicted blue line


#Finally, let's create an ACF and PACF plot of the residuals of our best fit ARIMA model i.e. ARIMA(0,1,1)(0,1,1)[12]. 
#The following is the R code for the same.

#Since there are no spikes outside the insignificant zone for both ACF and PACF plots we can conclude 
#that residuals are random with no information or juice in them. Hence our ARIMA model is working fine.
