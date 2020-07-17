#Package Used 
library(quantmod)
library(tseries)
library(astsa)
library(TSA)
library(forecast)
library(fGarch)
library("rugarch")


#Retrieve seasonally adjusted quarterly US debt-to-GDP ratio from FRED 

getSymbols('GFDEGDQ188S',src='FRED')
ratiodata<-GFDEGDQ188S
head(ratiodata)
colnames(ratiodata)[1]<-"Debt-to-GDP Ratio"
plot(ratiodata)
ratio<-ts(ratiodata[,1],frequency=4,start=c(1966,1))
head(ratio)
plot(ratio)


#Check for non-stationarity
acf(ratio)
adf.test(ratio,k=10)
##p>0.05; unit root
#1st Differencing 
ratio_d<-diff(ratio)
adf.test(ratio_d,k=10)
##p>0.05; unit root
#2nd Differencing
ratio_dd<-diff(ratio_d)
adf.test(ratio_dd,k=10)
##p<0.05; no unit root, stationary

acf(ratio_dd)
##Check for seasonality effect
ratio_sdd<-diff(ratio_dd,lag=4)
acf(ratio_sdd)
plot(ratio_sdd)


#Fit data into an adequate model
auto.arima(ratio)
mSARIMA<-arima(ratio,order=c(1,2,2),seasonal=list(order=c(0,0,2),period=4))
mSARIMA
##Proving the need for twice differencing and quarterly seasonal effect
tsdiag(mSARIMA)
##P-value remains considerably high until lag (quarter) 10
Box.test(resid(mSARIMA),type="Ljung",lag=20)
##P-value>5%; H0=Adequate model. Fail to Reject null hyptohesis.
### Adequate Model:SARIMA(1,2,2)(0,0,2)[4] 

#Heteroscedasticity test on the squared residual of SARIMA model
resid<-resid(mSARIMA)
head(resid)
par(mfcol=c(2,1))
acf(resid)
acf(resid^2)
##Acf of resid^2 does not diminish -> showing strong correlations between residuals square

#Fit GARCH(1,1)model on SARIMA residuals
mGARCH<-garchFit(~garch(1,1), data=resid,trace=F)
mGARCH
##Based on the result we can see significant ARCH and GARCH effect 

#Check for SARIMA-GARCH model adequacy
plot(mGARCH)
##Weak serial dependency between the residuals
##Weak serial dependency between the residuals^2 --> No longer has heteroscedasticity effect
###QQ plot shows less accurate at tails [HeavyTail Distributions]

#Predict the mean evolution of the SARIMA 
predM<-predict(mSARIMA,n.ahead=9)
head(predM)

#Predict the volatility evolution of the GARCH(1,1) 
predV<-predict(mGARCH,n.ahead=9)
head(predV)
predVpred<-predV[,1]
predVse<-predV[,2]
head(predVpred)
head(predVse)

#Combine both prediction [Mean evolution +Volatility evolution]
pred<-predM$pred+predVpred
se<-predM$se+predVse
head(pred)
head(se)

#Plot the forecast
dates<-seq(as.Date("1966/1/1"),by="quarter",length.out=length(ratio)+9)
plot(dates[1:length(ratio)],ratio,lwd=3,type="l",main="1-Step to 9-Step-Ahead Forecasts of US Quarterly Debt-to-GDP Ratio",xlab='Time',ylab='Ratio',ylim=c(0,115))
lines(dates[(length(ratio)+1):length(dates)],pred,col="blue", lwd=5)
lines(dates[(length(ratio)+1):length(dates)],pred+1.96*predVse,col="red",lty=3, lwd=5)
lines(dates[(length(ratio)+1):length(dates)],pred-1.96*predVse,col="red",lty=3, lwd=5)

#Zoom in 
dates<-seq(as.Date("2020/1/1"),by="quarter",length.out=9)
plot(dates[1:length(dates)],pred,col="blue",lwd=3,type="l",main="1-Step to 9-Step-Ahead Forecasts of US Quarterly Debt-to-GDP Ratio",xlab='Time',ylab='Ratio',ylim=c(90,115))
lines(dates[1:length(dates)],pred+1.96*predVse,col="red",lty=3, lwd=5)
lines(dates[1:length(dates)],pred-1.96*predVse,col="red",lty=3, lwd=5)





