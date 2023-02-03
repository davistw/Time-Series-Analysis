
library(TSA)
library(mgcv)


# Load data
data1 <- read.csv("Put path to data file here")
head(data1)
data1 <- data1[,2]

# Convert to TS data in proper frame
rate <- ts(data1,start=c(2000,1),freq=52)

# Generate differenced data
rate.dif1 <- diff(rate)



################ Currency Conversion Analysis ################




# Time Series and ACF Plots of Non-Differenced Data
plot(rate)

library(forecast)
acf(rate,lag.max=52*4,main="")

# Time Series and ACF Plots of Differenced Data
plot(rate.dif)

library(forecast)
acf(rate.dif,lag.max=52*4,main="")


# Categorical Linear Regression (ANOVA)

library(TSA)
week = season(rate)
# Drop First Week (model with intercept)
anova_rate1 = lm(rate~week)
rate.fit.anova_rate1 = ts(fitted(anova_rate1),start=2000,frequency=52)
# All seasonal mean effects (model without intercept)
anova_rate2 = lm(rate~week-1)
rate.fit.anova_rate2 = ts(fitted(anova_rate2),start=2000,frequency=52)
# Time Series, Residuals, and ACF Plots
ts.plot(rate,ylab="Rate",main='ANOVA With Intercept')
lines(rate.fit.anova_rate1,lwd=2,col="red")
ts.plot(rate,ylab="Rate",main='ANOVA Without Intercept')
lines(rate.fit.anova_rate2,lwd=2,col="blue")

resids.anova_rate1 = residuals(anova_rate1)
resids.ts.anova_rate1 = ts(resids.anova_rate1,start=2000,freq=52)
ts.plot(resids.ts.anova_rate1,main="ANOVA With Intercept")
abline(a=mean(resids.ts.anova_rate1),b=0,col='black')

acf(resids.ts.anova_rate1,main='ACF of ANOVA With Intercept')

resids.anova_rate2 = residuals(anova_rate2)
resids.ts.anova_rate2 = ts(resids.anova_rate2,start=2000,freq=52)
ts.plot(resids.ts.anova_rate2,main="ANOVA Without Intercept")
abline(a=mean(resids.ts.anova_rate2),b=0,col='black')

acf(resids.ts.anova_rate2,main='ACF of ANOVA Without Intercept')


# Non-parametric model (Local Polynomial Model)
time.pts2 = c(1:length(rate))
time.pts2 = c(time.pts2 - min(time.pts2))/max(time.pts2)
loc.fit2 = loess(rate~time.pts2)
rate.fit.loc = ts(fitted(loc.fit2),start=2000,frequency=52)

ts.plot(rate,ylab="Rate",main='Local Polynomial Model')
lines(rate.fit.loc,lwd=2,col="green")

# Residuals and ACF Plot
resids.loc_rate1 = rate-rate.fit.loc
resids.ts.loc_rate1 = ts(resids.loc_rate1,start=2000,freq=52)
ts.plot(resids.ts.loc_rate1,main="Local Polynomial Residuals")
abline(a=mean(resids.ts.loc_rate1),b=0,col='black')

acf(resids.ts.loc_rate1,main='ACF of Local Polynomial')


######### Application of Models to Differenced Data #########

rate.dif = ts(rate.dif1,start=c(2000,2),freq=52)


# Categorical Linear Regression (ANOVA)

library(TSA)
week = season(rate.dif)
# Drop First Week (model with intercept)
anova_rate1.dif = lm(rate.dif~week)
rate.fit.anova_rate1.dif = ts(fitted(anova_rate1.dif),start=2000,frequency=52)
# All seasonal mean effects (model without intercept)
anova_rate2.dif = lm(rate.dif~week-1)
rate.fit.anova_rate2.dif = ts(fitted(anova_rate2.dif),start=2000,frequency=52)
# Time Series, Residuals, and ACF Plots
ts.plot(rate.dif,ylab="Rate",main='ANOVA With Intercept')
lines(rate.fit.anova_rate1.dif,lwd=2,col="red")
ts.plot(rate.dif,ylab="Rate",main='ANOVA Without Intercept')
lines(rate.fit.anova_rate2.dif,lwd=2,col="blue")

resids.anova_rate1.dif = residuals(anova_rate1.dif)
resids.ts.anova_rate1.dif = ts(resids.anova_rate1.dif,start=2000,freq=52)
ts.plot(resids.ts.anova_rate1.dif,main="ANOVA With Intercept")
abline(a=mean(resids.ts.anova_rate1.dif),b=0,col='black')

acf(resids.ts.anova_rate1.dif,main='ACF of ANOVA With Intercept')

resids.anova_rate2.dif = residuals(anova_rate2.dif)
resids.ts.anova_rate2.dif = ts(resids.anova_rate2.dif,start=2000,freq=52)
ts.plot(resids.ts.anova_rate2.dif,main="ANOVA Without Intercept")
abline(a=mean(resids.ts.anova_rate2.dif),b=0,col='black')

acf(resids.ts.anova_rate2.dif,main='ACF of ANOVA Without Intercept')


# Non-parametric model (Local Polynomial Model)
time.pts3 = c(1:length(rate.dif))
time.pts3 = c(time.pts3 - min(time.pts3))/max(time.pts3)
loc.fit3 = loess(rate.dif~time.pts3)
rate.fit.loc2 = ts(fitted(loc.fit3),start=2000,frequency=52)

ts.plot(rate.dif,ylab="Rate",main='Local Polynomial Model')
lines(rate.fit.loc2,lwd=2,col="green")

# Residuals and ACF Plot
resids.loc_rate2 = rate.dif-rate.fit.loc2
resids.ts.loc_rate2 = ts(resids.loc_rate2,start=2000,freq=52)
ts.plot(resids.ts.loc_rate2,main="Local Polynomial Residuals")
abline(a=mean(resids.ts.loc_rate2),b=0,col='black')

acf(resids.ts.loc_rate2,main='ACF of Local Polynomial')
