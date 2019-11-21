mydata<-read.csv("C:/Users/Varsha C/Desktop/data analytics/train.csv",header=TRUE)
head(mydata)
mydata<-na.omit(mydata)
head(mydata)

install.packages('tseries')
require(tseries)
#testing if its stationary:

#plotting time series:
ts<-ts(mydata$sales,frequency = 12, start=c(2013,1), end=c(2019,1) )
plot(ts)
#ACF
ts_acf<-acf(ts,lag.max=NULL,plot=TRUE)
#PACF
ts_pacf<-pacf(ts,lag.max=NULL,plot=TRUE)
#stationary or not:
test<-adf.test(ts,alternative="stationary", k=0)
test
#MA MODEL
ma_fit<-arima(ts, order= c(0,0,1))
predict(ma_fit,n.ahead=11,se.fit=TRUE)           
#AR MODEL
ar_fit<-arima(ts, order= c(1,0,0))
predict(ar_fit,n.ahead=11,se.fit=TRUE)           
#BOX_TEST
test<-Box.test(residuals(ar_fit), lag=1.4, type="Ljung-Box")
test


test<-Box.test(residuals(ma_fit),lag=1.4, type = "Ljung-Box")
test





#HOLT WINTER MODEL
saleForecast<- HoltWinters(ts,beta=FALSE,gamma = FALSE)
saleForecast
plot(saleForecast)
library('forecast')
sf<-forecast(saleForecast, h=8, na.action= na.pass)
#library('forecast')
#install.packages('forecast')
accuracy(sf)
plot(sf)

#Holtwinters with seasonality:
season<-HoltWinters(x=ts)
season
accuracy(forecast(saleForecast,11))
accuracy(forecast(season,11))

#accuracy(ar_fit)
plot(forecast(saleForecast,11))
plot(forecast(ts,11))
plot(forecast(season,11))

#ar.ols
arols<-ar.ols(ts,qic=TRUE,order.max=1,demean = TRUE)
predict(arols,n.ahead=11,se.fit = TRUE)

#BOX_TEST
test<-Box.test(residuals(ar_fit), lag=24, type="Ljung-Box")
test


test<-Box.test(residuals(ma_fit),lag=24, type = "Ljung-Box")
test


test<-Box.test(residuals(saleForecast),lag=24,type="Ljung-Box")
test

test<-Box.test(residuals(season),lag=24,type="Ljung-Box")
test

test<-Box.test(residuals(arols),lag=24,type="Ljung-Box")
test
