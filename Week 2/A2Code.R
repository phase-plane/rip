wd <- paste0(getwd(),"/Week 2/")
setwd(wd)
remove(wd)

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