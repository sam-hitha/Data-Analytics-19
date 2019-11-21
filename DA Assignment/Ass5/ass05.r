mydata<-read.csv("C:/Users/Varsha C/Desktop/data analytics/train.csv",header=TRUE)
head(mydata)
lm<-lm(formula=sales~store+item,data=mydata)
summary(lm)
library("TTR")
library('forecast')
ts<-ts(mydata$sales,frequency=12,start=c(2013,1), end=c(2019,1))
de<-decompose(ts)
plot(de)
#finding rmse
rss<-c(crossprod(lm$residuals))
mse<-rss/length(lm$residuals)
rmse<-sqrt(mse)
rmse

#exponential smoothing:
s<-HoltWinters(ts,beta = FALSE, gamma=FALSE)
f2<-forecast(s,h=12)
accuracy(f2)
s
plot(f2)
#trend component:
s<-HoltWinters(ts,gamma=FALSE)
s
f3<-forecast(s,h=12)
accuracy(f3)
plot(f3)

#seasonality:
s<-HoltWinters(ts)
s
f<-forecast(s,h=12)
accuracy(f)
plot(f)

#random values
s<-HoltWinters(ts, gamma=0.3)
f2<-forecast(s,h=12)
accuracy(f2)
s
plot(f2)
