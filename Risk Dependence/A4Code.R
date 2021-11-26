setwd(paste0(getwd(),"/Risk Dependence/"))
rm(list = ls())

# Question 1

# import data
prices <- read.table(file="RMIP Data Tutorial.txt", header=TRUE)
prices$Date <- as.Date(prices$Date, '%m/%d/%Y')

# log returns
lr.NL <- diff(log(prices$Netherlands))
lr.GR <- diff(log(prices$Greece))
lr <- cbind(lr.NL, lr.GR)

# Question 2
# a)
n <- length(lr.NL) ## 5217
x <- lr.NL - mean(lr.NL)
m2 <- mean(x^2); m3 <- mean(x^3); m4 <- mean(x^4)
b <- (m3^2) / (m2^3)
k <- m4 / (m2^2)
jb <- 1/6 * n * (b + 1/4*(k-3)^2)
1-pchisq(jb, df=2) ## p-value

# b)
n <- length(lr.GR) 
x <- lr.GR - mean(lr.GR)
m2 <- mean(x^2); m3 <- mean(x^3); m4 <- mean(x^4)
b <- (m3^2) / (m2^3)
k <- m4 / (m2^2)
jb <- 1/6 * n * (b + 1/4*(k-3)^2)
1-pchisq(jb, df=2)

# Question 3
## a)
## normal Q-Q plot for the NL log-returns
samp.quant.NL <- quantile(lr.NL, probs = seq(0, 1, 1/n)) 
norm.quant.NL <- qnorm(seq(0, 1, 1/n), mean=mean(lr.NL), sd=sd(lr.NL))  
plot(norm.quant.NL, samp.quant.NL, xlim=range(samp.quant.NL), 
     main="Normal Q-Q plot Netherlands log-returns", xlab = "Normal", 
     ylab = "Sample") 
abline(0,1)

## b)
## normal Q-Q plot for the GR log-returns
samp.quant.GR <- quantile(lr.GR, probs = seq(0, 1, 1/n)) 
norm.quant.GR <- qnorm(seq(0, 1, 1/n), mean=mean(lr.GR), sd=sd(lr.GR))  
plot(norm.quant.GR, samp.quant.GR, xlim=range(samp.quant.GR), 
     main="Normal Q-Q plot Greece log-returns", xlab = "Normal", 
     ylab = "Sample") 
abline(0,1)

# Question 4
install.packages("QRM")
library(QRM)
MardiaTest(lr)

# Question 5
fit.norm(lr)
bivariate.lr <- fit.norm(lr)
mu <- as.numeric(bivariate.lr$mu)
Sigma <- as.matrix(bivariate.lr$Sigma)

## simulated bivariate normal log-returns
norm.lr <- rmnorm(n, mu, Sigma)
colnames(norm.lr) <- c("norm.lr.NL", "norm.lr.GR")

# Question 6
par(mfrow=c(1,2))
plot(lr, xlab="Netherlands", ylab="Greece") 
plot(norm.lr, xlim=range(lr.NL), ylim=range(lr.GR), xlab="Netherlands",
     ylab="Greece")
mtext("Scatterplot log-returns: Original vs Bivariate Normal", 
      line=-2, font=2, cex=1.2, outer=TRUE)

# Question 7
## a) 
lr.ranks.NL <- rank(lr.NL)

## b)
lr.ranks.GR <- rank(lr.GR)

lr.ranks.NL <- lr.ranks.NL/n
lr.ranks.GR <- lr.ranks.GR/n
lr.ranks <- cbind(lr.ranks.NL, lr.ranks.GR)

# Question 8
cor(lr.ranks)
Spearman(lr)

# Question 9
norm.lr.ranks <- rcopula.gauss(n, cor(lr.ranks))

## a)
par(mfrow=c(1,2))
plot(lr.ranks, xlab="Netherlands", ylab="Greece") 
plot(norm.lr.ranks,  xlab="Netherlands", ylab="Greece")
mtext("Empirical Copula vs Simulated Normal Copula", 
      line=-2, font=2, cex=1.2, outer=TRUE)

# Question 10
## a)
t.NL <- fit.st(lr.NL)
nu.NL <- t.NL$par.ests[1]
mu.NL <- t.NL$par.ests[2]
sigma.NL <- t.NL$par.ests[3]

## b)
t.GR <- fit.st(lr.GR)
nu.GR <- t.GR$par.ests[1]
mu.GR <- t.GR$par.ests[2]
sigma.GR <- t.GR$par.ests[3]

## c)
dst <- function(x,nu,mu,sigma){1/sigma*dt((x-mu)/sigma, nu)}
hist(lr.NL, prob=T, breaks=40, xlim=c(-0.06,0.06), ylim=c(0,50), 
     main="Histogram of Netherlands log-returns", xlab = expression(r[t]))
curve(dnorm(x, mean=mu.NL, sd=sigma.NL), add=T, col="red")
curve(dst(x, nu.NL, mu.NL, sigma.NL), add=T, col="blue")

## d)
hist(lr.GR, prob=T, breaks=40, xlim=c(-0.06,0.06), ylim=c(0,50), 
     main="Histogram of Greece log-returns", xlab = expression(r[t]))
curve(dnorm(x, mean=mu.GR, sd=sigma.GR), add=T, col="red")
curve(dst(x, nu.GR, mu.GR, sigma.GR), add=T, col="blue")

## Question 12
meta.lr.NL <- qst(norm.lr.ranks[,1], df=nu.NL, mu=mu.NL, sd=sigma.NL)
meta.lr.GR <- qst(norm.lr.ranks[,2], df=nu.GR, mu=mu.GR, sd=sigma.GR)
meta.lr <- cbind(meta.lr.NL, meta.lr.GR) ## the meta-normal bivariate data set

par(mfrow=c(1,2))
plot(lr, xlab="Netherlands", ylab="Greece") 
plot(meta.lr, xlim=range(lr.NL), ylim=range(lr.GR), xlab="Netherlands",
     ylab="Greece")
mtext("Scatterplot log-returns: Original vs Meta-Normal", 
      line=-2, font=2, cex=1.2, outer=TRUE)

## Question 13
par(mfrow=c(1,2))
qqplot(rowSums(norm.lr), rowSums(lr), xlim=range(rowSums(lr))); abline(0,1)
qqplot(rowSums(meta.lr), rowSums(lr), xlim=range(rowSums(lr))); abline(0,1)
mtext("Q-Q plot: Normal vs Meta-Normal", 
      line=-2, font=2, cex=1.2, outer=TRUE)