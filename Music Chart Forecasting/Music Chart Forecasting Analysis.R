
################ Music Chart Analysis ################



# Clears environment
rm(list=ls())
gc()


# Load Necessary Libraries
library(TSA)
library(mgcv)


# Load and check data file
data <- read.csv("Put path to data file here")
data <- data[,2]


# Convert to time series data in proper frame
chart = ts(data,start=c(1965,1),freq=12)

# Differenced data
chart.dif = diff(chart)





### Create Time Series and ACF Plots

# Time Series Plot
plot(chart)

# ACF Plot
library(forecast)
acf(chart, type=c("correlation"), plot=TRUE)

# Time Series Plot (Differenced Data)
plot(chart.dif)

# ACF Plot (Differenced Data)
library(forecast)
acf(chart.dif, type=c("correlation"), plot=TRUE)


### ARIMA Modeling

#ARIMA order
test_modelA <- function(p,d,q){
  mod = arima(chart, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:3){
  for (d in 0:1){
    for (q in 0:3) {
      possibleError <- tryCatch(
        orders<-rbind(orders,test_modelA(p,d,q)),
        error=function(e) e
      )
      if(inherits(possibleError, "error")) next
      
    }
  }
}
orders <- orders[order(-orders$AIC),]
tail(orders)

# Fitting ARIMA(3,1,3) Model

arima_model313 = arima(chart, order=c(3,1,3), method="ML")

# Residual Analysis

plot(resid(arima_model313), ylab='Standardized Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(as.vector(resid(arima_model313)),lag.max=365*2,main="ACF: Residuals")
hist(resid(arima_model313),xlab='Standardized Residuals',main='Histogram: Residuals')
qqnorm(resid(arima_model313))
qqline(resid(arima_model313))

# Ljung-Box Test

library(stats)
arima_vector1 = as.vector(resid(arima_model313))

Box.test(arima_vector1, type = c("Ljung-Box"))


# Fitting ARIMA(2,1,4) Model

arima_model214 = arima(chart, order=c(2,1,4), method="ML")
summary(arima_model214)

# 6 Month Forecast and Comparison to Actual Values

n = length(chart)
nfit = n-6
outchart = arima(chart[1:nfit], order = c(2,1,4),method = "ML")
out_pred = as.vector(predict(outchart,n.ahead=6))

timechart=time(chart)
ubound = out_pred$pred+1.96*out_pred$se
lbound = out_pred$pred-1.96*out_pred$se
ymin = min(lbound)
ymax = max(ubound)
plot(timechart[(n-15):n],chart[(n-15):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Unique Songs")
points(timechart[(nfit+1):n],out_pred$pred,col="red")
lines(timechart[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timechart[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")

# Compute MAE, MAPE, and PM 

predicted1 = out_pred$pred
actual1 = as.vector(chart[643:648])

mean(abs(predicted1-actual1))
mean(abs(predicted1-actual1)/actual1)
sum((predicted1-actual1)^2)/sum((actual1-mean(actual1))^2)