
################ Multivariate Currency Conversion Analysis (USD, EUR, GBP, AUD, NZD) ################



# Clears environment
rm(list=ls())
gc()


# Load Necessary Libraries
library(TSA)
library(data.table)
library(vars)

# Load and check data file
data <- read.csv("Put path to data file here")
head(data)

# Split into Train and Test
train <- data[0:(length(data[,1])-8),]
train.ts <- ts(train[,c(2,3,4,5)],start=c(2014,1),freq=52)
test <- data[(length(data[,1])-7):length(data[,1]),]


# Needed for plotting predictions
whole <- ts(data$USD.EU,start=c(2014,1),freq=52)
times = time(whole)
times.test = tail(times,8)
rm(whole)

#USD/EUR
EU.train <- ts(train$USD.EU,start=c(2014,1),freq=52)
EU.test <- test$USD.EU

#USD/GBP
GBP.train <- ts(train$USD.GBP,start=c(2014,1),freq=52)
GBP.test <- test$USD.GBP

#USD/AU
AU.train <- ts(train$USD.AU,start=c(2014,1),freq=52)
AU.test <- test$USD.AU

#USD/NZ
NZ.train <- ts(train$USD.NZ,start=c(2014,1),freq=52)
NZ.test <- test$USD.NZ


# Differenced Data

EU.train.diff = diff(EU.train)
GBP.train.diff = diff(GBP.train)
AU.train.diff = diff(AU.train)
NZ.train.diff = diff(NZ.train)

# Plot Differenced Data
plot(EU.train.diff)
plot(GBP.train.diff)
plot(AU.train.diff)
plot(NZ.train.diff)


# ACF Plots for Differenced Data
acf(EU.train.diff)
acf(GBP.train.diff)
acf(AU.train.diff)
acf(NZ.train.diff)


# EACF Plots for Univariate Analysis

eacf(EU.train.diff,7,7)
eacf(GBP.train.diff,7,7)
eacf(AU.train.diff,7,7)
eacf(NZ.train.diff,7,7)

# ARIMA Order Selection Via Iteration

# EU
test_modelA <- function(p,d,q){
  mod = arima(EU.train, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:5){
  for (d in 0:1){
    for (q in 0:5) {
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



# GBP
test_modelA <- function(p,d,q){
  mod = arima(GBP.train, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:5){
  for (d in 0:1){
    for (q in 0:5) {
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




# AU
test_modelA <- function(p,d,q){
  mod = arima(AU.train, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:5){
  for (d in 0:1){
    for (q in 0:5) {
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




# NZ
test_modelA <- function(p,d,q){
  mod = arima(NZ.train, order=c(p,d,q), method="ML")
  current.aic = AIC(mod)
  df = data.frame(p,d,q,current.aic)
  names(df) <- c("p","d","q","AIC")
  print(paste(p,d,q,current.aic,sep=" "))
  return(df)
}

orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")


for (p in 0:5){
  for (d in 0:1){
    for (q in 0:5) {
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




### Fit ARIMA Models for EU, GBP, AU, and NZ and Make Predictions

# Create Complete Time Series For Each Currency (Differenced for EU and GBP)
whole_EU <- ts(data$USD.EU,start=c(2014,1),freq=52)
whole_GBP <- ts(data$USD.GBP,start=c(2014,1),freq=52)
whole_AU <- ts(data$USD.AUD,start=c(2014,1),freq=52)
whole_NZ <- ts(data$USD.NZ,start=c(2014,1),freq=52)



# EU
n = length(whole_EU)
nfit = n-8
outcurrency = arima(EU.train, order = c(2,1,3),method = "ML")
out_pred_EU = as.vector(predict(outcurrency,n.ahead=8))

timecurrency= time(whole_EU)
ubound = out_pred_EU$pred+1.96*out_pred_EU$se
lbound = out_pred_EU$pred-1.96*out_pred_EU$se
ymin = min(lbound)
ymax = max(ubound)
plot(timecurrency[(n-10):n],whole_EU[(n-10):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Exchange Rate EU")
points(timecurrency[(nfit+1):n],out_pred_EU$pred,col="red")
lines(timecurrency[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timecurrency[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")




# GBP
n = length(whole_GBP)
nfit = n-8
outcurrency = arima(GBP.train, order = c(1,1,1),method = "ML")
out_pred_GBP = as.vector(predict(outcurrency,n.ahead=8))

timecurrency= time(whole_GBP)
ubound = out_pred_GBP$pred+1.96*out_pred_GBP$se
lbound = out_pred_GBP$pred-1.96*out_pred_GBP$se
ymin = min(lbound)
ymax = max(ubound)
plot(timecurrency[(n-10):n],whole_GBP[(n-10):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Exchange Rate GBP")
points(timecurrency[(nfit+1):n],out_pred_GBP$pred,col="red")
lines(timecurrency[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timecurrency[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")




# AU
n = length(whole_AU)
nfit = n-8
outcurrency = arima(AU.train, order = c(1,0,3),method = "ML")
out_pred_AU = as.vector(predict(outcurrency,n.ahead=8))

timecurrency= time(whole_AU)
ubound = out_pred_AU$pred+1.96*out_pred_AU$se
lbound = out_pred_AU$pred-1.96*out_pred_AU$se
ymin = min(lbound)
ymax = max(ubound)
plot(timecurrency[(n-10):n],whole_AU[(n-10):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Exchange Rate AU")
points(timecurrency[(nfit+1):n],out_pred_AU$pred,col="red")
lines(timecurrency[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timecurrency[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")


# NZ
n = length(whole_NZ)
nfit = n-8
outcurrency = arima(NZ.train, order = c(2,0,2),method = "ML")
out_pred_NZ = as.vector(predict(outcurrency,n.ahead=8))

timecurrency= time(whole_NZ)
ubound = out_pred_NZ$pred+1.96*out_pred_NZ$se
lbound = out_pred_NZ$pred-1.96*out_pred_NZ$se
ymin = min(lbound)
ymax = max(ubound)
plot(timecurrency[(n-10):n],whole_NZ[(n-10):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Exchange Rate NZ")
points(timecurrency[(nfit+1):n],out_pred_NZ$pred,col="red")
lines(timecurrency[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timecurrency[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")



# Model MAPE Value Computations

predicted_EU = as.numeric(out_pred_EU$pred)
actual_EU = as.numeric(EU.test)

predicted_GBP = as.numeric(out_pred_GBP$pred)
actual_GBP = as.numeric(GBP.test)

predicted_AU = as.numeric(out_pred_AU$pred)
actual_AU = as.numeric(AU.test)

predicted_NZ = as.numeric(out_pred_NZ$pred)
actual_NZ = as.numeric(NZ.test)

print(predicted_EU)
print(actual_EU)

100*mean(abs(predicted_EU-actual_EU)/actual_EU)
100*mean(abs(predicted_GBP-actual_GBP)/actual_GBP)
100*mean(abs(predicted_AU-actual_AU)/actual_AU)
100*mean(abs(predicted_NZ-actual_NZ)/actual_NZ)



# VAR Selection via AIC

var_selection <- VARselect(train.ts)
var_selection$selection

# Create VAR(2) Model

Var_model_2 <- VAR(train.ts, p=2)


# ARCH Test
arch.test(Var_model_2)

# Jarque-Bera Test
normality.test(Var_model_2)

# Portmanteau Test
serial.test(Var_model_2)


### VAR 8 Month Forecast with CI and MAPE Calculations

preds.all <- as.vector(predict(Var_model_2,n.ahead=8))

## EU
preds.EU <- as.numeric(preds.all$fcst$USD.EU[,1])
obs.EU <- as.numeric(EU.test)
print(preds.EU)

# CI (Upper and Lower Bounds)
ci.EU.upper <- as.vector(preds.all$fcst$USD.EU[,3])
ci.EU.lower <- as.vector(preds.all$fcst$USD.EU[,2])
print(ci.EU.upper)
print(ci.EU.lower)

# MAPE
100*mean(abs(preds.EU-obs.EU)/obs.EU)


## GBP

preds.GBP <- as.numeric(preds.all$fcst$USD.GBP[,1])
obs.GBP <- as.numeric(GBP.test)
print(preds.GBP)

# CI (Upper and Lower Bounds)
ci.GBP.upper <- as.vector(preds.all$fcst$USD.GBP[,3])
ci.GBP.lower <- as.vector(preds.all$fcst$USD.GBP[,2])
print(ci.GBP.upper)
print(ci.GBP.lower)

# MAPE
100*mean(abs(preds.GBP-obs.GBP)/obs.GBP)


## AU

preds.AU <- as.numeric(preds.all$fcst$USD.AUD[,1])
obs.AU <- as.numeric(AU.test)
print(preds.AU)

# CI (Upper and Lower Bounds)
ci.AU.upper <- as.vector(preds.all$fcst$USD.AUD[,3])
ci.AU.lower <- as.vector(preds.all$fcst$USD.AUD[,2])
print(ci.AU.upper)
print(ci.AU.lower)

# MAPE
100*mean(abs(preds.AU-obs.AU)/obs.AU)


## NZ

preds.NZ <- as.numeric(preds.all$fcst$USD.NZ[,1])
obs.NZ <- as.numeric(NZ.test)
print(preds.NZ)

# CI (Upper and Lower Bounds)
ci.NZ.upper <- as.vector(preds.all$fcst$USD.NZ[,3])
ci.NZ.lower <- as.vector(preds.all$fcst$USD.NZ[,2])
print(ci.NZ.upper)
print(ci.NZ.lower)

# MAPE
100*mean(abs(preds.NZ-obs.NZ)/obs.NZ)



