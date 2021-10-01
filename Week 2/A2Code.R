wd <- paste0(getwd(),"/Week 2/")
setwd(wd)
remove(wd)
remove(list = ls())

# import data
Data <- read.table(file="RMIP Data Tutorial.txt", header=TRUE)

# convert to date format to be used as the X-axis
Data$Date <- as.Date(Data$Date, '%m/%d/%Y')

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

# Question 1

# losses
gr.losses <- greece.lr * -1 * 100
nl.losses <- netherlands.lr * -1 * 100
combined.losses <- nl.losses + gr.losses
rm(netherlands.lr)
rm(greece.lr)

# 1-day 95% VaR 
var.gr <- quantile(gr.losses, probs = c(.95))
var.nl <- quantile(nl.losses, probs = c(.95))
var.combined <- quantile(combined.losses, probs = c(.95))

# allocation
allocate.nl <- ((var.combined)/(var.nl + var.gr)) * var.nl
allocate.gr <- ((var.combined)/(var.nl + var.gr)) * var.gr

# covariance method
variance.losses <- var(combined.losses)
cov.greece <- cov(gr.losses, combined.losses)
cov.nl <- cov(nl.losses, combined.losses)

# allocation 2
allocate2.gr <- (cov.greece / variance.losses) * var.combined
allocate2.nl <- (cov.nl / variance.losses) * var.combined

# check
allocate2.gr + allocate2.nl - var.combined

# 1C: quantile allocation

# loss vectors ordered
gr.ordered <- gr.losses[order(gr.losses)]
nl.ordered <- nl.losses[order(nl.losses)]

# comonotone sum
comonotone <- nl.ordered + gr.ordered

# recall the VaR of combined losses variable
var.combined                                # 4.17731

# quantiles of comonotone sum
a <- quantile(comonotone, probs = c(0.93))  # 4.090931 
b <- quantile(comonotone, probs = c(0.94))  # 4.446706

#  quantile allocation continued
w <- quantile(nl.losses, probs = c(0.93))
x <- quantile(nl.losses, probs = c(0.94))
y <- quantile(gr.losses, probs = c(0.93))
z <- quantile(gr.losses, probs = c(0.94))

alpha <- (var.combined - (x + z)) / ((w + y) - (x + z))
allocationC.nl <- alpha * w + (1 - alpha) * x
allocationC.gr <- alpha * y + (1 - alpha) * z

# CTE
var.combined

# (L > VaR)
events <- which(combined.losses > var.combined)
greece.cte <- gr.losses[events]
nl.cte <- nl.losses[events]

# denominator 
cte <- (1 / 0.05) * (mean(greece.cte) + mean(nl.cte))

# allocations
cte.allocation.nl <- (var.combined / cte) * (mean(nl.cte)/.05)
cte.allocation.gr <- (var.combined / cte) * (mean(greece.cte)/.05)

# a)
# preliminaries
mu <- c(mean(nl.losses), mean(gr.losses))
covariance.l <- cov(nl.losses, gr.losses)
nl.variance <- var(nl.losses)
gr.variance <- var(gr.losses)

# establish distribution parameters
a <- c(1,1)
Covmat <- rbind(c(nl.variance, covariance.l),
                c(covariance.l, gr.variance))

l.mu <- t(a) %*% mu
l.sig2 <- t(a) %*% Covmat %*% a

# 1-day 95% VaR 
alpha <- .95
var.nl <- qnorm(alpha, mean = mean(nl.losses), sd = sqrt(nl.variance))
var.gr <- qnorm(alpha, mean = mean(gr.losses), sd = sqrt(gr.variance))
var.losses <- qnorm(alpha, mean = l.mu, sd = sqrt(l.sig2))

# allocation
allocate.nl <- ((var.losses)/(var.nl + var.gr)) * var.nl
allocate.gr <- ((var.losses)/(var.nl + var.gr)) * var.gr

# covariance method
variance.losses <- l.sig2
cov.nl <- nl.variance + covariance.l
cov.greece <- gr.variance + covariance.l

# allocation: covariance (parametric)
allocate.nl <- (cov.nl / variance.losses) * var.losses
allocate.gr <- (cov.greece / variance.losses) * var.losses

# check
allocate.gr + allocate.nl - var.losses

# Part 2c)
# parametric quantile allocation

# recall the comonotone sum variable
comonotone

# and the risk measure to be allocated
var.losses                                # 4.293338

# quantiles of comonotone sum
p.star <- 0.932
p.hat <- 0.938

quantile(comonotone, probs = c(p.star))  # 4.090931 
quantile(comonotone, probs = c(p.hat))   # 4.446706

#  quantile allocation continued
w <- qnorm(p.star, mean = mean(nl.losses), sd = sqrt(nl.variance))
x <- qnorm(p.hat, mean = mean(nl.losses), sd = sqrt(nl.variance))
y <- qnorm(p.star, mean = mean(gr.losses), sd = sqrt(gr.variance))
z <- qnorm(p.hat, mean = mean(gr.losses), sd = sqrt(gr.variance))

alpha <- (var.losses - (x + z)) / ((w + y) - (x + z))
allocation.nl <- alpha * w + (1 - alpha) * x
allocation.gr <- alpha * y + (1 - alpha) * z

# check
allocation.gr + allocation.nl - var.losses

# Part 2d)
# CTE

# recall the variables from 2b)
cov.nl <- nl.variance + covariance.l
cov.greece <- gr.variance + covariance.l

# allocations
alpha <- .95
cte.nl <- (mean(nl.losses) + (cov.nl / sqrt(l.sig2)) * 
             (dnorm(qnorm(alpha)) / (1-alpha)))

cte.gr <- (mean(gr.losses) + (cov.greece / sqrt(l.sig2)) * 
             (dnorm(qnorm(alpha)) / (1-alpha)))

cte.total = cte.nl + cte.gr
cte.allocation.nl <- (cte.nl / cte.total) * var.losses
cte.allocation.gr <- (cte.gr / cte.total) * var.losses

# check
cte.allocation.gr + cte.allocation.nl - var.losses