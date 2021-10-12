setwd(paste0(getwd(),"/Risk Measures/"))
library(TSA)

## Question 1

## (a)
## data
data(deere3)

## plot the time series
plot(deere3, xlab = "t", type="o", main="Chemical Process", 
     ylab="Deviation")
abline(0, 0, col ="royalblue", lty=2)

## (b)
## parameters
par(mfrow=c(1,2), mar=c(6,5,2,1), oma=c(0,0,3,0))

## display ACF and PACF => ARMA(1,1)
acf(deere3, main="")
pacf(deere3, main="")

## (c)
arma.1 <- arima(deere3, order = c(1,0,1))
arma.1$coef

## (d)
acf(residuals(arma.1), main="")
pacf(residuals(arma.1), main="")
LB.test(arma.1)

## (e)
par(mfrow= c(1,1))
qqnorm(residuals(arma.1)); qqline(residuals(arma.1))

## (f)
ma.1 <- arima(deere3, order = c(0,0,1))
ma.1$coef
AIC(arma.1, ma.1) ## arma

# Question 2
Data <- read.table(file="RMIP Data Tutorial.txt", header=TRUE);
Data$Date <- as.Date(Data$Date, '%m/%d/%Y');

## log returns
netherlands.lr <- diff(log(Data$Netherlands))
greece.lr <- diff(log(Data$Greece))

## plot log returns
par(mfrow=c(1,1))
plot(netherlands.lr, xlab="Time", type="l", 
     ylab=expression(r[t]), main="Netherlands log-returns")

## (a) clustering = yes

## (b) 
library(tseries)
arch.1 <- garch(netherlands.lr, order = c(0,1), trace = FALSE)
arch.1$coef

residuals.arch <- na.remove(residuals(arch.1))
acf(residuals.arch^2)
pacf(residuals.arch^2)
LB.test(arch.1)
qqnorm(residuals.arch); qqline(residuals.arch)

## (c) predictive volatility
pv <- arch.1$coef[1] + arch.1$coef[2] * 
  (residuals.arch[length(residuals.arch)])^2

## (d) VaR
alpha <- .95
sigma <- sqrt(pv)
value.at.risk <- sigma * qnorm(alpha)