
# The code below loads and prepares our "LA Temp Monthly" data for analysis.

# Clears environment

rm(list=ls())

gc()

# Load Necessary Libraries

library(TSA)
library(mgcv)

# Load and check data file

data <- read.csv("Put path to data file here")
head(data)
data <- data[,2]

# Convert to time series data in proper frame

temp <- ts(data,start=c(1950,1),freq=12)
head(temp)


################ Temperature Analysis (Los Angeles) ################


# Create Time Series and ACF Plots

# Time Series Plot
plot(temp)

# ACF Plot
library(forecast)
acf(temp,lag.max=12*4,main="")


### Trend Models


# Moving Average Model
time.pts = c(1:length(temp))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
mav.fit = ksmooth(time.pts, temp, kernel = "box")
temp.fit.mav = ts(mav.fit$y,start=1950,frequency=12)

# Parametric Quadratic Polynomial Model
x1 = time.pts
x2 = time.pts^2
lm.fit = lm(temp~x1+x2)
temp.fit.lm = ts(fitted(lm.fit),start=1950,frequency=12)

# Local Polynomial Model
loc.fit = loess(temp~time.pts)
temp.fit.loc = ts(fitted(loc.fit),start=1950,frequency=12)

# Splines Model
library(mgcv)
gam.fit = gam(temp~s(time.pts))
temp.fit.gam = ts(fitted(gam.fit),start=1950,frequency=12)


# Plot of Fitted Models
ts.plot(temp,ylab="Temperature")
lines(temp.fit.mav,lwd=2,col="red")
lines(temp.fit.lm, lwd=2, col="green")
lines(temp.fit.loc, lwd=2, col="blue")
lines(temp.fit.gam, lwd=2, col="purple")
abline(temp.fit.mav[1],0,lwd=2,col="black")


### Residuals and ACF plots

# Moving Average
resids.mav = temp-temp.fit.mav
resids.ts.mav = ts(resids.mav,start=1950,freq=12)
ts.plot(resids.ts.mav,main="Moving Average Residuals")
abline(a=mean(resids.ts.mav),b=0,col='red')

acf(resids.ts.mav,lag.max=12*4,main='ACF of Moving Average Residuals')


# Parametric Quadratic Polynomial
resids.lm = residuals(lm.fit)
resids.ts.lm = ts(resids.lm,start=1950,freq=12)
ts.plot(resids.ts.lm,main="Parametric Quadratic Polynomial Residuals")
abline(a=mean(resids.ts.lm),b=0,col='red')

acf(resids.ts.lm,lag.max=12*4,main='ACF of Parametric Quadratic Polynomial Residuals')


# Local Polynomial
resids.loc = residuals(loc.fit)
resids.ts.loc = ts(resids.loc,start=1950,freq=12)
ts.plot(resids.ts.loc,main="Local Polynomial Residuals")
abline(a=mean(resids.ts.loc),b=0,col='red')

acf(resids.ts.loc,main='ACF of Local Polynomial Residuals')


# Splines
resids.gam = residuals(gam.fit)
resids.ts.gam = ts(resids.loc,start=1950,freq=12)
ts.plot(resids.ts.gam,main="Splines Residuals")
abline(a=mean(resids.ts.gam),b=0,col='red')

acf(resids.ts.gam,lag.max=12*4,main='ACF of Splines Residuals')



### Seasonality Estimation Models

# Categorical Linear Regression (ANOVA)
library(TSA)
month = season(temp)
# Drop January (model with intercept)
anova1 = lm(temp~month)
temp.fit.anova1 = ts(fitted(anova1),start=1950,frequency=12)
# All seasonal mean effects (model without intercept)
anova2 = lm(temp~month-1)
temp.fit.anova2 = ts(fitted(anova2),start=1950,frequency=12)
# Time Series, Residuals, and ACF Plots
ts.plot(temp,ylab="Temperature",main='ANOVA With Intercept')
lines(temp.fit.anova1,lwd=2,col="red")
ts.plot(temp,ylab="Temperature",main='ANOVA Without Intercept')
lines(temp.fit.anova2,lwd=2,col="blue")

resids.anova = residuals(anova1)
resids.ts.anova = ts(resids.anova,start=1950,freq=12)
ts.plot(resids.ts.anova,main="ANOVA With Intercept")
abline(a=mean(resids.ts.anova),b=0,col='black')

acf(resids.ts.anova,main='ACF of ANOVA With Intercept')

resids.anova2 = residuals(anova2)
resids.ts.anova2 = ts(resids.anova2,start=1950,freq=12)
ts.plot(resids.ts.anova2,main="ANOVA Without Intercept")
abline(a=mean(resids.ts.anova2),b=0,col='black')

acf(resids.ts.anova2,main='ACF of ANOVA Without Intercept')


# Cos_Sin Model
# Estimate seasonality using cos-sin model
har=harmonic(temp,1)
cos_sin1=lm(temp~har)
temp.fit.cos_sin1 = ts(fitted(cos_sin1),start=1950,frequency=12)
har2=harmonic(temp,2)
cos_sin2=lm(temp~har2)
temp.fit.cos_sin2 = ts(fitted(cos_sin2),start=1950,frequency=12)
# Time Series, Residuals, and ACF Plots
ts.plot(temp,ylab="Temperature",main="Cos_Sin1")
lines(temp.fit.cos_sin1,lwd=2,col="green")
ts.plot(temp,ylab="Temperature",main="Cos_Sin2")
lines(temp.fit.cos_sin1,lwd=2,col="yellow")

resids.cos_sin = residuals(cos_sin1)
resids.ts.cos_sin = ts(resids.cos_sin,start=1950,freq=12)
ts.plot(resids.ts.cos_sin,main="Cos_Sin2")
abline(a=mean(resids.ts.cos_sin),b=0,col='black')

acf(resids.ts.cos_sin,main='ACF of Cos_Sin1')

resids.cos_sin2 = residuals(cos_sin2)
resids.ts.cos_sin2 = ts(resids.cos_sin2,start=1950,freq=12)
ts.plot(resids.ts.cos_sin2,main="Cos_Sin2")
abline(a=mean(resids.ts.cos_sin2),b=0,col='black')

acf(resids.ts.cos_sin2,main='ACF of Cos_Sin2')













