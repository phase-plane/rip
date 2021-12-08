# Extreme Value Theory

## Question 1

# import data
prices <- read.table(file="RMIP Data Tutorial.txt", header=TRUE)
prices$Date <- as.Date(prices$Date, '%m/%d/%Y')

# values
values <- (prices$Netherlands+prices$Greece)/2

# losses
losses <- 100*(1-Quot(values))                ## see package DescTools

# plot
plot(prices$Date[-1], losses, type="l", xlab="", ylab="Loss",
     main ="Daily Percentage Losses", ylim=range(losses))

## Question 2
maxima <- NA
blocks <- split(losses, ceiling(seq_along(losses)/65))
for(i in 1:80) {maxima[i] <- max(unlist(blocks[i]))}

## Question 3
library(QRM)
parameters <- fit.GEV(maxima)
xi.GEV <- parameters$par.ests[1]
mu.GEV <- parameters$par.ests[2]
sigma.GEV <- parameters$par.ests[3]

## Question 4
neg.loglik <- function(par) {-sum(log(dGEV(maxima, par[1], par[2], par[3])))}
x <- 0; m <- 0; s <- 1
o1 <- optim(c(x,m,s), neg.loglik)
xi.o1 <- o1$par[1]; mu.o1 <- o1$par[2]; sigma.o1 <- o1$par[3];
round(abs(c(xi.o1-xi.GEV, mu.GEV-mu.o1, sigma.GEV-sigma.o1)), 5)

## Question 5
hist(maxima, prob=T, ylim=c(0,0.35))
curve(dGEV(x, xi=xi.GEV, mu=mu.GEV, sigma=sigma.GEV), lwd=2, col="red", add=T)

## Question 6
1/(1-pGEV(10, xi.GEV, mu.GEV, sigma.GEV))

## Question 7
MEplot(losses[losses>0], omit=25, pch=16)
fit.GPD(losses, threshold=0.5)
fitted.gpd <- fit.GPD(losses, threshold=0.5)
xi.GPD <- fitted.gpd$par.ests[1]
beta.GPD <- fitted.gpd$par.ests[2]

## Question 8
excesses <- losses[losses>0.5]-0.5
neg.loglik <- function(par) {-sum(log(dGPD(excesses, par[1], par[2])))}
x <- 0; b <- 1;
o2 <- optim(c(x,b), neg.loglik)
xi.o2 <- o2$par[1]; beta.o2 <- o2$par[2]; 
round(abs(c(xi.o2-xi.GPD, beta.o2-beta.GPD)), 5)

## Question 9
plotFittedGPDvsEmpiricalExcesses(losses, threshold=0.5)

## Question 10
u <- 0.5; xi <- xi.GPD; beta <- beta.GPD; p <- 0.995;
n <- length(losses); cap.N <- length(losses[losses>u])
val.risk <- u + (beta/xi)*((n*(1-p)/cap.N)^(-xi)-1)

## verify
quantile(losses, probs=.995)

## (b)
exp.short <- as.numeric(val.risk/(1-xi) + (beta-xi*u)/(1-xi))

## verify
q <- as.numeric(quantile(losses, probs=.995))
empirical.es <- mean(losses[losses>q])

## Question 11
xiplot(losses, start=1300, end=1600, models=200, lwd=2)
abline(v=-sum(losses>0.5))