wd <- paste0(getwd(),"/Risk Measures/")
setwd(wd)
remove(wd)
# Question 1
# a)
sigma <- 5e6 * 0.01 * sqrt(10)
VaR <- 0 +  sigma * qnorm(.99)

# b)
mu.p <- 0
sigma.m <- 2e6 * 0.02 * sqrt(10)
sigma.a <- 5e6 * 0.01 * sqrt(10)
sigma.p2 <- ((2/7)^2 * (sigma.m)^2 + (5/7)^2 * (sigma.a)^2 + 
               2*(2/7)*(5/7)*0.3*sigma.m*sigma.a)

VaR.p <- mu.p + sqrt(sigma.p2) * qnorm(.99)
VaR.p

# Question 2
# import data
Data <- read.table(file="RMIP Data Tutorial.txt", header=TRUE);

# convert to date format to be used as the X-axis
Data$Date <- as.Date(Data$Date, '%m/%d/%Y');

# Identifying largest Index for plotting purposes
index.max <- which.max(Data$Greece)
value.max <- Data$Greece[index.max]

# plot Dutch price dynamics
plot(Data$Date, Data$Netherlands, type = "l",
     col = "blue", xlab = "Date", ylab = "Price" ,
     main = "MSCI Prices", ylim = c(0, value.max));

# plot Greek price dynamics
lines(Data$Date, Data$Greece, type = "l", col = "red");

# include legends 
legend("topleft", c("Netherlands","Greece"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c(" blue" ,"red"))

# nl log returns
netherlands.lr <- diff(log(Data$Netherlands))

# gr log returns
greece.lr <- diff(log(Data$Greece))

# 2.1 Compute 10 Day .99 VaR Netherlands
h <- 10 
mean.nl <- mean(netherlands.lr)
sd.nl <- sd(netherlands.lr) 
var.nl <- qnorm(.99, mean = mean.nl*h, sd = sd.nl * sqrt(h))

# 2.2 Compute the 10 Day 99% Expected Shortfall Netherlands
alpha <- .99
es.nl <- (mean.nl*h +
   sd.nl*sqrt(h) * (dnorm(qnorm(alpha))/ (1-alpha)))

# 2.3 Compute 10 Day .99 VaR, ES Greece

# VaR
mean.gr <- mean(greece.lr)
sd.gr <- sd(greece.lr) 
var.gr <- qnorm(.99, mean = mean.gr*h, sd = sd.gr*sqrt(h))

# Expected Shortfall
alpha <- .99
es.gr <- (mean.gr*h +
             sd.gr*sqrt(h) * (dnorm(qnorm(alpha))/ (1-alpha)))

# 2.4 Generate plots of VaR and ES for varying alpha
#  .95 Netherlands
alpha <- .95
mean.nl <- mean(netherlands.lr)
sd.nl <- sd(netherlands.lr) 
var.nl <- qnorm(alpha, mean = mean.nl, sd = sd.nl)
es.nl <- (mean.nl +
             sd.nl * (dnorm(qnorm(alpha))/ (1-alpha)))
# plot
par(mfrow=c(1,1))
hist(netherlands.lr, freq = FALSE, breaks=40, ylim = c(0,50), 
     main="Netherlands log-returns (.95 level)", xlab =expression('r'[t]))
curve(dnorm(x, mean=mean(netherlands.lr), sd=sd(netherlands.lr)),
      add=TRUE, col="red")
abline(v=var.nl, col="blue")
abline(v=es.nl, col="forestgreen")

# include legends 
legend("topleft", c("VaR","ES"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"forestgreen"))

#  .99 VaR Netherlands
alpha <- .99
var.nl <- qnorm(alpha, mean = mean.nl, sd = sd.nl)
es.nl <- (mean.nl +
             sd.nl * (dnorm(qnorm(alpha))/ (1-alpha)))

# plot
par(mfrow=c(1,1))
hist(netherlands.lr, freq = FALSE, breaks=40, ylim = c(0,50), 
     main="Netherlands log-returns (.99 level)", xlab =expression('r'[t]))
curve(dnorm(x, mean=mean(netherlands.lr), sd=sd(netherlands.lr)),
      add=TRUE, col="red")
abline(v=var.nl, col="blue")
abline(v=es.nl, col="forestgreen")

# include legends 
legend("topleft", c("VaR","ES"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"forestgreen"))

#  .9975 VaR Netherlands
alpha <- .9975
var.nl <- qnorm(alpha, mean = mean.nl, sd = sd.nl)
es.nl <- (mean.nl +
             sd.nl * (dnorm(qnorm(alpha))/ (1-alpha)))

# plot
par(mfrow=c(1,1))
hist(netherlands.lr, freq = FALSE, breaks=40, ylim = c(0,50), 
     main="Netherlands log-returns (.9975 level)", xlab =expression('r'[t]))
curve(dnorm(x, mean=mean(netherlands.lr), sd=sd(netherlands.lr)),
      add=TRUE, col="red")
abline(v=var.nl, col="blue")
abline(v=es.nl, col="forestgreen")

# include legends 
legend("topleft", c("VaR","ES"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"forestgreen"))

# plot log returns
par(mfrow=c(1,3))

plot(netherlands.lr, greece.lr, main="Scatterplot log-returns", 
     xlab = "Netherlands", ylab = "Greece")

plot(netherlands.lr, xlab="Time", type="l", 
     ylab=expression('r'[t]), main="Netherlands log-returns")

plot(greece.lr, xlab="Time", type="l", 
     ylab=expression('r'[t]), main="Greece log-returns")

# histogram vs density
par(mfrow=c(1,1))
hist(netherlands.lr, freq = FALSE, breaks=40, ylim = c(0,50), 
     main="Histogram of log-returns", xlab =expression('r'[t]))
curve(dnorm(x, mean=mean(netherlands.lr), sd=sd(netherlands.lr)),
      add=TRUE, col="red")
lines(density(netherlands.lr), col="blue")

# include legends 
legend("topleft", c("Empirical","Theoretical"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c(" blue" ,"red"))