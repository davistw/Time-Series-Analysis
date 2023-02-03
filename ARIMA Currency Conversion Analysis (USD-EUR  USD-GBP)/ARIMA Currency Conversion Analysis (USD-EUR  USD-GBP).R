
################ Currency Conversion ARIMA Analysis ################



# Clears environment
rm(list=ls())
gc()


# Load Necessary Libraries
library(TSA)
library(mgcv)


# Load and check data file
data1 <- read.csv("Put path to data file here")
head(data1)
data1 <- data1[,2]

EU = ts(data1,start=c(2014),freq=52)

# Load and check data file
data2 <- read.csv("Put path to data file here")
head(data2)
data2 <- data2[,2]

GBP = ts(data2,start=c(2014),freq=52)





### ARIMA Modeling

# ARIMA order (EU)
test_modelA <- function(p,d,q){
  mod = arima(EU, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:3){
  for (d in 0:2){
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


### ARIMA Modeling

# ARIMA order (GBP)
test_modelA <- function(p,d,q){
  mod = arima(GBP, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:3){
  for (d in 0:2){
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


# Fitting ARIMA(2,1,2) Model (USD-EU)

arima_model212 = arima(EU, order=c(2,1,2), method="ML")

# Residual Analysis

plot(resid(arima_model212), ylab='Standardized Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(as.vector(resid(arima_model212)),lag.max=365*2,main="ACF: Residuals")
hist(resid(arima_model212),xlab='Standardized Residuals',main='Histogram: Residuals')
qqnorm(resid(arima_model212))
qqline(resid(arima_model212))

# Ljung-Box Test (Arima(2,1,2))

library(stats)
arima_vector2 = as.vector(resid(arima_model212))

Box.test(arima_vector2, type = c("Ljung-Box"))



# Fitting ARIMA(2,1,0) Model (USD-GBP)

arima_model210 = arima(GBP, order=c(2,1,0), method="ML")

# Residual Analysis

plot(resid(arima_model210), ylab='Standardized Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(as.vector(resid(arima_model210)),lag.max=365*2,main="ACF: Residuals")
hist(resid(arima_model210),xlab='Standardized Residuals',main='Histogram: Residuals')
qqnorm(resid(arima_model210))
qqline(resid(arima_model210))

# Ljung-Box Test (Arima(2,1,0))

library(stats)
arima_vector3 = as.vector(resid(arima_model210))

Box.test(arima_vector3, type = c("Ljung-Box"))




# Fitting ARIMA(2,1,1) Model (USD - EU)

arima_model211 = arima(EU, order=c(2,1,1), method="ML")
summary(arima_model211)

# 12 Week Forecast and Comparison to Actual Values

n = length(EU)
nfit = n-12
outcurrency = arima(EU[1:nfit], order = c(2,1,1),method = "ML")
out_pred = as.vector(predict(outcurrency,n.ahead=12))

timecurrency=time(EU)
ubound = out_pred$pred+1.96*out_pred$se
lbound = out_pred$pred-1.96*out_pred$se
ymin = min(lbound)
ymax = max(ubound)
plot(timecurrency[(n-25):n],EU[(n-25):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Exchange Rate")
points(timecurrency[(nfit+1):n],out_pred$pred,col="red")
lines(timecurrency[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timecurrency[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")

# Compute MAE, MAPE, and PM 

predicted2 = out_pred$pred
actual2 = as.vector(EU[277:288])

mean(abs(predicted2-actual2))
mean(abs(predicted2-actual2)/actual2)
sum((predicted2-actual2)^2)/sum((actual2-mean(actual2))^2)





# Fitting ARIMA(2,1,0) Model (USD - GBP)

arima_model210 = arima(GBP, order=c(2,1,0), method="ML")
summary(arima_model210)

# 6 Month Forecast and Comparison to Actual Values

n = length(GBP)
nfit = n-12
outcurrency = arima(GBP[1:nfit], order = c(2,1,0),method = "ML")
out_pred = as.vector(predict(outcurrency,n.ahead=12))

timecurrency=time(GBP)
ubound = out_pred$pred+1.96*out_pred$se
lbound = out_pred$pred-1.96*out_pred$se
ymin = min(lbound)
ymax = max(ubound)
plot(timecurrency[(n-25):n],GBP[(n-25):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Exchange Rate")
points(timecurrency[(nfit+1):n],out_pred$pred,col="red")
lines(timecurrency[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timecurrency[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")

# Compute MAE, MAPE, and PM 

predicted3 = out_pred$pred
actual3 = as.vector(GBP[277:288])

mean(abs(predicted3-actual3))
mean(abs(predicted3-actual3)/actual3)
sum((predicted3-actual3)^2)/sum((actual3-mean(actual3))^2)
