library(ggplot2)
library(tsbox)
library(tibble)
library(reshape2)
library(gridExtra)
library(BSDA)
library(ggcorrplot)
library(SMFI5)
library(sde)
library(fitdistrplus)
library(Sim.DiffProc)
library(stats4)
library(lawstat)
library(TTR)
library(actuar)

install.packages("TTR")


cad_1m = read.csv(".\\Project\\CAD1MTD156N.csv")
cad_3m = read.csv(".\\Project\\CAD3MTD156N.csv")
cad_6m = read.csv(".\\Project\\CAD6MTD156N.csv")
cad_12m = read.csv(".\\Project\\CAD12MD156N.csv")

usd_1m = read.csv(".\\Project\\USD1MTD156N.csv")
usd_3m = read.csv(".\\Project\\USD3MTD156N.csv")
usd_6m = read.csv(".\\Project\\USD6MTD156N.csv")
usd_12m = read.csv(".\\Project\\USD12MD156N.csv")

# attach(cad_1m)
# names(cad_1m)
# head(cad_1m)
# detach(cad_1m)

# sum(is.na(CAD1MTD156N))
# cad_1m[is.na(CAD1MTD156N),]
# min(CAD1MTD156N, na.rm = TRUE)
# max(CAD1MTD156N, na.rm = TRUE)


############################################# DATA CLEANING ###################################################### 
names(cad_1m) <- c("time", "value")
names(cad_3m) <- c("time", "value")
names(cad_6m) <- c("time", "value")
names(cad_12m) <- c("time", "value")

names(usd_1m) <- c("time", "value")
names(usd_3m) <- c("time", "value")
names(usd_6m) <- c("time", "value")
names(usd_12m) <- c("time", "value")


typeof(cad_1m$time)
class(cad_1m$time)
typeof(cad_1m$value)
class(cad_1m$value)

cad_1m$time <- as.Date(paste(substr(cad_1m$time, 1, 4), substr(cad_1m$time, 6, 7), substr(cad_1m$time, 9, 10), sep = "-"))
cad_1m$value <- as.numeric(levels(cad_1m$value))[cad_1m$value]
cad_3m$time <- as.Date(paste(substr(cad_3m$time, 1, 4), substr(cad_3m$time, 6, 7), substr(cad_3m$time, 9, 10), sep = "-"))
cad_3m$value <- as.numeric(levels(cad_3m$value))[cad_3m$value]
cad_6m$time <- as.Date(paste(substr(cad_6m$time, 1, 4), substr(cad_6m$time, 6, 7), substr(cad_6m$time, 9, 10), sep = "-"))
cad_6m$value <- as.numeric(levels(cad_6m$value))[cad_6m$value]
cad_12m$time <- as.Date(paste(substr(cad_12m$time, 1, 4), substr(cad_12m$time, 6, 7), substr(cad_12m$time, 9, 10), sep = "-"))
cad_12m$value <- as.numeric(levels(cad_12m$value))[cad_12m$value]

usd_1m$time <- as.Date(paste(substr(usd_1m$time, 1, 4), substr(usd_1m$time, 6, 7), substr(usd_1m$time, 9, 10), sep = "-"))
usd_1m$value <- as.numeric(levels(usd_1m$value))[usd_1m$value]
usd_3m$time <- as.Date(paste(substr(usd_3m$time, 1, 4), substr(usd_3m$time, 6, 7), substr(usd_3m$time, 9, 10), sep = "-"))
usd_3m$value <- as.numeric(levels(usd_3m$value))[usd_3m$value]
usd_6m$time <- as.Date(paste(substr(usd_6m$time, 1, 4), substr(usd_6m$time, 6, 7), substr(usd_6m$time, 9, 10), sep = "-"))
usd_6m$value <- as.numeric(levels(usd_6m$value))[usd_6m$value]
usd_12m$time <- as.Date(paste(substr(usd_12m$time, 1, 4), substr(usd_12m$time, 6, 7), substr(usd_12m$time, 9, 10), sep = "-"))
usd_12m$value <- as.numeric(levels(usd_12m$value))[usd_12m$value]


###################################### IR TIME SERIES ###########################################################


# time series plotting

ggplot(cad_1m, aes(x = cad_1m$time, name="Time")) +
  geom_line(aes(y = cad_1m$value, col="CAD_1M"), size = 0.5) +
  geom_line(aes(y = cad_3m$value, col="CAD_3M"), size = 0.5) +
  geom_line(aes(y = cad_6m$value, col="CAD_6M"), size = 0.5) +
  geom_line(aes(y = cad_12m$value, col="CAD_12M"), size = 0.5) +
  scale_colour_manual("", 
                      breaks = c("CAD_1M", "CAD_3M", "CAD_6M", "CAD_12M"),
                      values = c("black", "red", "blue", "green")) +
  xlab("Time")+
  ylab("Interest Rate (%)") +
  ggtitle("CAD LIBOR Rate - Time Series") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

ggplot(usd_1m, aes(x = usd_1m$time, name="Time")) +
  geom_line(aes(y = usd_1m$value, col="USD_1M"), size = 0.5) +
  geom_line(aes(y = usd_3m$value, col="USD_3M"), size = 0.5) +
  geom_line(aes(y = usd_6m$value, col="USD_6M"), size = 0.5) +
  geom_line(aes(y = usd_12m$value, col="USD_12M"), size = 0.5) +
  scale_colour_manual("", 
                      breaks = c("USD_1M", "USD_3M", "USD_6M", "USD_12M"),
                      values = c("black", "red", "blue", "green")) +
  xlab("Time")+
  ylab("Interest Rate (%)") +
  ggtitle("USD LIBOR Rate - Time Series") + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

########################################## IR Densities ########################################################

windows(12, 16)
par(mfrow=c(4,2))

hist(cad_1m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="CAD IR Density plot - 1 Month Tenor")
lines(density(na.omit(cad_1m$value)), col="red")

hist(usd_1m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="USD IR Density plot - 1 Month Tenor")
lines(density(na.omit(usd_1m$value)), col="red")


hist(cad_3m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="CAD IR Density plot - 3 Month Tenor")
lines(density(na.omit(cad_3m$value)), col="red")

hist(usd_3m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="USD IR Density plot - 3 Month Tenor")
lines(density(na.omit(usd_3m$value)), col="red")


hist(cad_6m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="CAD IR Density plot - 6 Month Tenor")
lines(density(na.omit(cad_6m$value)), col="red")

hist(usd_6m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="USD IR Density plot - 6 Month Tenor")
lines(density(na.omit(usd_6m$value)), col="red")


hist(cad_12m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="CAD IR Density plot - 12 Month Tenor")
lines(density(na.omit(cad_12m$value)), col="red")

hist(usd_12m$value, breaks=200, xlim=c(0, 12.5), freq=F, 
     xlab="Interest Rate (%)", main="USD IR Density plot - 12 Month Tenor")
lines(density(na.omit(usd_12m$value)), col="red")  


  
  
###################################### INCREMENT CALCULATIONS #####################################################

cad_1m <- within(cad_1m, diff_1d <- c(NA,diff(cad_1m$value)))
cad_1m <- within(cad_1m, diff_10d <- c(NA[1:10],diff(cad_1m$value, 10)))
cad_1m <- within(cad_1m, diff_25d <- c(NA[1:25],diff(cad_1m$value, 25)))
cad_1m <- within(cad_1m, diff_90d <- c(NA[1:90],diff(cad_1m$value, 90)))

cad_3m <- within(cad_3m, diff_1d <- c(NA,diff(cad_3m$value)))
cad_3m <- within(cad_3m, diff_10d <- c(NA[1:10],diff(cad_3m$value, 10)))
cad_3m <- within(cad_3m, diff_25d <- c(NA[1:25],diff(cad_3m$value, 25)))
cad_3m <- within(cad_3m, diff_90d <- c(NA[1:90],diff(cad_3m$value, 90)))

cad_6m <- within(cad_6m, diff_1d <- c(NA,diff(cad_6m$value)))
cad_6m <- within(cad_6m, diff_10d <- c(NA[1:10],diff(cad_6m$value, 10)))
cad_6m <- within(cad_6m, diff_25d <- c(NA[1:25],diff(cad_6m$value, 25)))
cad_6m <- within(cad_6m, diff_90d <- c(NA[1:90],diff(cad_6m$value, 90)))

cad_12m <- within(cad_12m, diff_1d <- c(NA,diff(cad_12m$value)))
cad_12m <- within(cad_12m, diff_10d <- c(NA[1:10],diff(cad_12m$value, 10)))
cad_12m <- within(cad_12m, diff_25d <- c(NA[1:25],diff(cad_12m$value, 25)))
cad_12m <- within(cad_12m, diff_90d <- c(NA[1:90],diff(cad_12m$value, 90)))


usd_1m <- within(usd_1m, diff_1d <- c(NA,diff(usd_1m$value)))
usd_1m <- within(usd_1m, diff_10d <- c(NA[1:10],diff(usd_1m$value, 10)))
usd_1m <- within(usd_1m, diff_25d <- c(NA[1:25],diff(usd_1m$value, 25)))
usd_1m <- within(usd_1m, diff_90d <- c(NA[1:90],diff(usd_1m$value, 90)))

usd_3m <- within(usd_3m, diff_1d <- c(NA,diff(usd_3m$value)))
usd_3m <- within(usd_3m, diff_10d <- c(NA[1:10],diff(usd_3m$value, 10)))
usd_3m <- within(usd_3m, diff_25d <- c(NA[1:25],diff(usd_3m$value, 25)))
usd_3m <- within(usd_3m, diff_90d <- c(NA[1:90],diff(usd_3m$value, 90)))

usd_6m <- within(usd_6m, diff_1d <- c(NA,diff(usd_6m$value)))
usd_6m <- within(usd_6m, diff_10d <- c(NA[1:10],diff(usd_6m$value, 10)))
usd_6m <- within(usd_6m, diff_25d <- c(NA[1:25],diff(usd_6m$value, 25)))
usd_6m <- within(usd_6m, diff_90d <- c(NA[1:90],diff(usd_6m$value, 90)))

usd_12m <- within(usd_12m, diff_1d <- c(NA,diff(usd_12m$value)))
usd_12m <- within(usd_12m, diff_10d <- c(NA[1:10],diff(usd_12m$value, 10)))
usd_12m <- within(usd_12m, diff_25d <- c(NA[1:25],diff(usd_12m$value, 25)))
usd_12m <- within(usd_12m, diff_90d <- c(NA[1:90],diff(usd_12m$value, 90)))


####################################### INCREMENT TIME SERIES ##########################################################

# 1D

t1 = ggplot(cad_1m, aes(x = cad_1m$time , y = cad_1m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (1 Month Tenor)") +
  ylab("Increment (%)") +
  ggtitle("CAD LIBOR Rate - 1 Day Increments Time Series") +
  theme(plot.title = element_text(hjust = 0.5))
t2 = ggplot(cad_3m, aes(x = cad_3m$time , y = cad_3m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (3 Month Tenor)") +
  ylab("Increment (%)")
t3 = ggplot(cad_6m, aes(x = cad_6m$time , y = cad_6m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (6 Month Tenor)") +
  ylab("Increment (%)")
t4 = ggplot(cad_12m, aes(x = cad_12m$time , y = cad_12m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (12 Month Tenor)") +
  ylab("Increment (%)")


u1 = ggplot(usd_1m, aes(x = usd_1m$time , y = usd_1m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (1 Month Tenor)") +
  ylab("Increment (%)") +
  ggtitle("USD LIBOR Rate - 1 Day Increments Time Series") +
  theme(plot.title = element_text(hjust = 0.5))
u2 = ggplot(usd_3m, aes(x = usd_3m$time , y = usd_3m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (3 Month Tenor)") +
  ylab("Increment (%)")
u3 = ggplot(usd_6m, aes(x = usd_6m$time , y = usd_6m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (6 Month Tenor)") +
  ylab("Increment (%)")
u4 = ggplot(usd_12m, aes(x = usd_12m$time , y = usd_12m$diff_1d)) +
  geom_line(size = 0.5) +
  xlab("Time (12 Month Tenor)") +
  ylab("Increment (%)")

windows(12, 16)
grid.arrange(t1, u1, t2, u2, t3, u3, t4, u4, nrow = 4)


# 25D

t5 = ggplot(cad_1m, aes(x = cad_1m$time , y = cad_1m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (1 Month Tenor)") +
  ylab("Increment (%)") +
  ggtitle("CAD LIBOR Rate - 25 Day Increments Time Series") +
  theme(plot.title = element_text(hjust = 0.5))
t6 = ggplot(cad_3m, aes(x = cad_3m$time , y = cad_3m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (3 Month Tenor)") +
  ylab("Increment (%)")
t7 = ggplot(cad_6m, aes(x = cad_6m$time , y = cad_6m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (6 Month Tenor)") +
  ylab("Increment (%)")
t8 = ggplot(cad_12m, aes(x = cad_12m$time , y = cad_12m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (12 Month Tenor)") +
  ylab("Increment (%)")


u5 = ggplot(usd_1m, aes(x = usd_1m$time , y = usd_1m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (1 Month Tenor)") +
  ylab("Increment (%)") +
  ggtitle("USD LIBOR Rate - 25 Day Increments Time Series") +
  theme(plot.title = element_text(hjust = 0.5))
u6 = ggplot(usd_3m, aes(x = usd_3m$time , y = usd_3m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (3 Month Tenor)") +
  ylab("Increment (%)")
u7 = ggplot(usd_6m, aes(x = usd_6m$time , y = usd_6m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (6 Month Tenor)") +
  ylab("Increment (%)")
u8 = ggplot(usd_12m, aes(x = usd_12m$time , y = usd_12m$diff_25d)) +
  geom_line(size = 0.5) +
  xlab("Time (12 Month Tenor)") +
  ylab("Increment (%)")

windows(12, 16)
grid.arrange(t5, u5, t6, u6, t7, u7, t8, u8, nrow = 4)


###################################### INCREMENT DENSITIES #################################################

# 1D

windows(12, 16)
par(mfrow=c(4,2))

hist(cad_1m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="CAD IR Density plot - 1 Month Tenor")
lines(density(na.omit(cad_1m$diff_1d)), col="red")

hist(usd_1m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="USD IR Density plot - 1 Month Tenor")
lines(density(na.omit(usd_1m$diff_1d)), col="red")


hist(cad_3m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="CAD IR Density plot - 3 Month Tenor")
lines(density(na.omit(cad_3m$diff_1d)), col="red")

hist(usd_3m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="USD IR Density plot - 3 Month Tenor")
lines(density(na.omit(usd_3m$diff_1d)), col="red")


hist(cad_6m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="CAD IR Density plot - 6 Month Tenor")
lines(density(na.omit(cad_6m$diff_1d)), col="red")

hist(usd_6m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="USD IR Density plot - 6 Month Tenor")
lines(density(na.omit(usd_6m$diff_1d)), col="red")


hist(cad_12m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="CAD IR Density plot - 12 Month Tenor")
lines(density(na.omit(cad_12m$diff_1d)), col="red")

hist(usd_12m$diff_1d, breaks=1000, xlim=c(-0.2, 0.2), ylim=c(0,120), freq=F, 
     xlab="IR 1 Day Increments (%)", main="USD IR Density plot - 12 Month Tenor")
lines(density(na.omit(usd_12m$diff_1d)), col="red")


# 25D

windows(12, 16)
par(mfrow=c(4,2))

hist(cad_1m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="CAD IR Density plot - 1 Month Tenor")
lines(density(na.omit(cad_1m$diff_25d)), col="red")

hist(usd_1m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="USD IR Density plot - 1 Month Tenor")
lines(density(na.omit(usd_1m$diff_25d)), col="red")


hist(cad_3m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="CAD IR Density plot - 3 Month Tenor")
lines(density(na.omit(cad_3m$diff_25d)), col="red")

hist(usd_3m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="USD IR Density plot - 3 Month Tenor")
lines(density(na.omit(usd_3m$diff_25d)), col="red")


hist(cad_6m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="CAD IR Density plot - 6 Month Tenor")
lines(density(na.omit(cad_6m$diff_25d)), col="red")

hist(usd_6m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="USD IR Density plot - 6 Month Tenor")
lines(density(na.omit(usd_6m$diff_25d)), col="red")


hist(cad_12m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="CAD IR Density plot - 12 Month Tenor")
lines(density(na.omit(cad_12m$diff_25d)), col="red")

hist(usd_12m$diff_25d, breaks=500, xlim=c(-1, 1), ylim=c(0,15), freq=F, 
     xlab="IR 25 Day Increments (%)", main="USD IR Density plot - 12 Month Tenor")
lines(density(na.omit(usd_12m$diff_25d)), col="red")



# USD vs CAD 1d density and 25d density 

windows(12, 16)
par(mfrow=c(4,2))

plot(density(sample(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)], 5000)), xlim=c(-0.2, 0.2), 
     xlab="IR 1 Day Increments (%)", main="USD vs CAD IR Density plot - 1 Month Tenor")
lines(density(sample(cad_1m$diff_1d[complete.cases(cad_1m$diff_1d)], 5000)), col='red')
legend(0.12, 500, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

plot(density(sample(usd_1m$diff_25d[complete.cases(usd_1m$diff_25d)], 5000)), xlim=c(-1, 1), 
     xlab="IR 25 Day Increments (%)", main="USD vs CAD IR Density plot - 1 Month Tenor")
lines(density(sample(cad_1m$diff_25d[complete.cases(cad_1m$diff_25d)], 5000)), col='red')
legend(0.6, 6.5, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)


plot(density(sample(usd_3m$diff_1d[complete.cases(usd_3m$diff_1d)], 5000)), xlim=c(-0.15, 0.15), 
     xlab="IR 1 Day Increments (%)", main="USD vs CAD IR Density plot - 3 Month Tenor")
lines(density(sample(cad_3m$diff_1d[complete.cases(cad_3m$diff_1d)], 5000)), col='red')
legend(0.1, 200, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

plot(density(sample(usd_3m$diff_25d[complete.cases(usd_3m$diff_25d)], 5000)), xlim=c(-1, 1), 
     xlab="IR 25 Day Increments (%)", main="USD vs CAD IR Density plot - 3 Month Tenor")
lines(density(sample(cad_3m$diff_25d[complete.cases(cad_3m$diff_25d)], 5000)), col='red')
legend(0.6, 4, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)


plot(density(sample(usd_6m$diff_1d[complete.cases(usd_6m$diff_1d)], 5000)), xlim=c(-0.2, 0.2), 
     xlab="IR 1 Day Increments (%)", main="USD vs CAD IR Density plot - 6 Month Tenor")
lines(density(sample(cad_6m$diff_1d[complete.cases(cad_6m$diff_1d)], 5000)), col='red')
legend(0.12, 75, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

plot(density(sample(usd_6m$diff_25d[complete.cases(usd_6m$diff_25d)], 5000)), xlim=c(-1.5, 1.5), 
     xlab="IR 25 Day Increments (%)", main="USD vs CAD IR Density plot - 6 Month Tenor")
lines(density(sample(cad_6m$diff_25d[complete.cases(cad_6m$diff_25d)], 5000)), col='red')
legend(1, 3, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)


plot(density(sample(usd_12m$diff_1d[complete.cases(usd_12m$diff_1d)], 5000)), xlim=c(-0.2, 0.2), 
     xlab="IR 1 Day Increments (%)", main="USD vs CAD IR Density plot - 12 Month Tenor")
lines(density(sample(cad_12m$diff_1d[complete.cases(cad_12m$diff_1d)], 5000)), col='red')
legend(0.1, 45, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

plot(density(sample(usd_12m$diff_25d[complete.cases(usd_12m$diff_25d)], 5000)), xlim=c(-1.5, 1.5), 
     xlab="IR 25 Day Increments (%)", main="USD vs CAD IR Density plot - 12 Month Tenor")
lines(density(sample(cad_12m$diff_25d[complete.cases(cad_12m$diff_25d)], 5000)), col='red')
legend(1, 2, legend=c("USD", "CAD"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)





############################################# 1D vs 25D OF 1M CAD AND 1M USD ############################################

plot(density(sample(cad_1m$diff_1d[complete.cases(cad_1m$diff_1d)], 5000)), xlim=c(-0.5, 0.5), 
     xlab="IR Increments (%)", main="CAD IR 1D vs 25D Increments Density plot - 1 Month Tenor")
lines(density(sample(cad_1m$diff_25d[complete.cases(cad_1m$diff_25d)], 5000)), col='red')
legend(0.18, 80, legend=c("CAD 1D", "CAD 25D"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)
legend(0.18, 55, legend=c("1D: mu=-0.0021, sd=0.0866", "25D: mu=-0.0537, sd=0.4078"), cex=0.6)

# sd(cad_1m$diff_1d[complete.cases(cad_1m$diff_1d)])
# mean(cad_1m$diff_1d[complete.cases(cad_1m$diff_1d)])
# sd(cad_1m$diff_25d[complete.cases(cad_1m$diff_25d)])
# mean(cad_1m$diff_25d[complete.cases(cad_1m$diff_25d)])


plot(density(sample(cad_1m$diff_1d[complete.cases(cad_1m$diff_1d)], 5000)), xlim=c(-0.2, 0.2), 
     xlab="IR Increments (%)", main="USD IR 1D vs 25D Increments Density plot - 1 Month Tenor")
lines(density(sample(cad_1m$diff_25d[complete.cases(cad_1m$diff_25d)], 5000)), col='red')
legend(0.07, 80, legend=c("USD 1D", "USD 25D"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)
legend(0.07, 55, legend=c("1D: mu=-0.0007, sd=0.0564", "25D: mu=-0.0174, sd=0.3274"), cex=0.6)

# sd(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)])
# mean(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)])
# sd(usd_1m$diff_25d[complete.cases(usd_1m$diff_25d)])
# mean(usd_1m$diff_25d[complete.cases(usd_1m$diff_25d)])

################################################## CORRPLOTS ################################################

# CAD

corr_1d <- data.frame("CAD 1M" = cad_1m$diff_1d, "CAD 3M" = cad_3m$diff_1d, "CAD 6M" = cad_6m$diff_1d, "CAD 12M" = cad_12m$diff_1d)
cormat_1d <- cor(na.omit(corr_1d)) # Gives a matrix
melted_cormat_1d <- melt(cormat_1d)

corr_10d <- data.frame("CAD 1M" = cad_1m$diff_10d, "CAD 3M" = cad_3m$diff_10d, "CAD 6M" = cad_6m$diff_10d, "CAD 12M" = cad_12m$diff_10d)
cormat_10d <- cor(na.omit(corr_10d)) # Gives a matrix
melted_cormat_10d <- melt(cormat_10d)

corr_25d <- data.frame("CAD 1M" = cad_1m$diff_25d, "CAD 3M" = cad_3m$diff_25d, "CAD 6M" = cad_6m$diff_25d, "CAD 12M" = cad_12m$diff_25d)
cormat_25d <- cor(na.omit(corr_25d)) # Gives a matrix
melted_cormat_25d <- melt(cormat_25d)

corr_90d <- data.frame("CAD 1M" = cad_1m$diff_90d, "CAD 3M" = cad_3m$diff_90d, "CAD 6M" = cad_6m$diff_90d, "CAD 12M" = cad_12m$diff_90d)
cormat_90d <- cor(na.omit(corr_90d)) # Gives a matrix
melted_cormat_90d <- melt(cormat_90d)


p1 <- ggcorrplot(cormat_1d, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
           title = "CAD LIBOR 1 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

p2 <- ggcorrplot(cormat_10d, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
           title = "CAD LIBOR 10 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

p3 <- ggcorrplot(cormat_25d, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
           title = "CAD LIBOR 25 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

p4 <- ggcorrplot(cormat_90d, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
           title = "CAD LIBOR 90 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

# p1 <- ggplot(data = melted_cormat_1d, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# p2 <- ggplot(data = melted_cormat_10d, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# p3 <- ggplot(data = melted_cormat_25d, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# p4 <- ggplot(data = melted_cormat_90d, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()

windows(10,6)
grid.arrange(p1, p2, p3, p4, nrow = 2)


# USD

corr_1d_usd <- data.frame("USD 1M" = usd_1m$diff_1d, "USD 3M" = usd_3m$diff_1d, "USD 6M" = usd_6m$diff_1d, "USD 12M" = usd_12m$diff_1d)
cormat_1d_usd <- cor(na.omit(corr_1d_usd)) # Gives a matrix
melted_cormat_1d_usd <- melt(cormat_1d_usd)

corr_10d_usd <- data.frame("USD 1M" = usd_1m$diff_10d, "USD 3M" = usd_3m$diff_10d, "USD 6M" = usd_6m$diff_10d, "USD 12M" = usd_12m$diff_10d)
cormat_10d_usd <- cor(na.omit(corr_10d_usd)) # Gives a matrix
melted_cormat_10d_usd <- melt(cormat_10d_usd)

corr_25d_usd <- data.frame("USD 1M" = usd_1m$diff_25d, "USD 3M" = usd_3m$diff_25d, "USD 6M" = usd_6m$diff_25d, "USD 12M" = usd_12m$diff_25d)
cormat_25d_usd <- cor(na.omit(corr_25d_usd)) # Gives a matrix
melted_cormat_25d_usd <- melt(cormat_25d_usd)

corr_90d_usd <- data.frame("USD 1M" = usd_1m$diff_90d, "USD 3M" = usd_3m$diff_90d, "USD 6M" = usd_6m$diff_90d, "USD 12M" = usd_12m$diff_90d)
cormat_90d_usd <- cor(na.omit(corr_90d_usd)) # Gives a matrix
melted_cormat_90d_usd <- melt(cormat_90d_usd)


# p5 <- ggplot(data = melted_cormat_1d_usd, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# p6 <- ggplot(data = melted_cormat_10d_usd, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# p7 <- ggplot(data = melted_cormat_25d_usd, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# p8 <- ggplot(data = melted_cormat_90d_usd, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()


p5 <- ggcorrplot(cormat_1d_usd, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
                 title = "USD LIBOR 1 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

p6 <- ggcorrplot(cormat_10d_usd, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
                 title = "USD LIBOR 10 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

p7 <- ggcorrplot(cormat_25d_usd, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
                 title = "USD LIBOR 25 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())

p8 <- ggcorrplot(cormat_90d_usd, lab = TRUE, ggtheme = ggplot2::theme_gray, colors = c("grey", "white", "darkgoldenrod1"), 
                 title = "USD LIBOR 90 Day Increment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major=element_blank())


windows(10,6)
grid.arrange(p5, p6, p7, p8, nrow = 2)

######################################## WELCH 2-SAMPLE T.TEST SIMULATION #################################################

par(mfrow=c(1,1))

# qqnorm(cad_1m$diff_1d)
# qqnorm(cad_1m$diff_25d)
# data.frame(na.omit(cad_1m$diff_1d[0:5804]), na.omit(cad_1m$diff_25d))




t.test(sample(cad_1m$diff_1d, 30),sample(cad_1m$diff_25d, 30))
t.test(sample(cad_1m$diff_1d, 30),sample(cad_1m$diff_25d, 30))$statistic

#qq plot showing difference between the distributions of 1d_inc and 25d_inc
#(slightly different shape, different parameters(mean and sd))


#### 1 MONTH TENOR ####

windows(12, 16)
par(mfrow=c(2,2))

#cad 1d vs 25d
ts = replicate(1000,t.test(sample(cad_1m$diff_1d, 30),sample(cad_1m$diff_25d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - CAD ", 
       xlab="1D vs 25D Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)

#usd 1d vs 25d
ts = replicate(1000,t.test(sample(usd_1m$diff_1d, 30),sample(usd_1m$diff_25d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - USD ", 
       xlab="1D vs 25D Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)

#cad 1d vs usd 1d
ts = replicate(1000,t.test(sample(cad_1m$diff_1d, 30),sample(usd_1m$diff_1d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - 1-Day Increment ", 
       xlab="USD vs CAD Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)

#cad 25d vs usd 25d
ts = replicate(1000,t.test(sample(cad_1m$diff_25d, 30),sample(usd_1m$diff_25d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - 25-Day Increment ", 
       xlab="USD vs CAD Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)



##### 6 MONTH TENOR ####

windows(12, 16)
par(mfrow=c(2,2))

#cad 1d vs 25d
ts = replicate(1000,t.test(sample(cad_6m$diff_1d, 30),sample(cad_6m$diff_25d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - CAD ", 
       xlab="1D vs 25D Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)

#usd 1d vs 25d
ts = replicate(1000,t.test(sample(usd_6m$diff_1d, 30),sample(usd_6m$diff_25d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - USD ", 
       xlab="1D vs 25D Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)

#cad 1d vs usd 1d
ts = replicate(1000,t.test(sample(cad_6m$diff_1d, 30),sample(usd_6m$diff_1d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - 1-Day Increment ", 
       xlab="USD vs CAD Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)

#cad 25d vs usd 25d
ts = replicate(1000,t.test(sample(cad_6m$diff_25d, 30),sample(usd_6m$diff_25d, 30))$statistic)
qqplot(ts,rt(1000,df=56), main="Welch's 2-Sample T-statistic - 25-Day Increment ", 
       xlab="USD vs CAD Sample Quantiles", ylab="T Distribution Sample Quantiles")
abline(0,1)




# P-VALUE DISTRIBUTIONS
ts = replicate(100,t.test(sample(cad_1m$diff_1d, 30),sample(cad_1m$diff_25d, 30))$p.value)
ts2 = replicate(100,t.test(sample(cad_3m$diff_1d, 30),sample(cad_3m$diff_25d, 30))$p.value)
ts3 = replicate(100,t.test(sample(cad_6m$diff_1d, 30),sample(cad_6m$diff_25d, 30))$p.value)
ts4 = replicate(100,t.test(sample(cad_12m$diff_1d, 30),sample(cad_12m$diff_25d, 30))$p.value)


plot(density(ts))
lines(density(ts2), col='red')
lines(density(ts3), col='blue')
lines(density(ts4), col='green')

# QQ PLOT OF P-VALUE SIMULATIONS
qqnorm(ts); qqline(ts)
qqnorm(rt(100,df=56)); qqline(rt(100,df=56))
qqplot(ts,rt(100,df=58))
abline(0,1)

# qqplot(ts2,rt(1000,df=58))
# abline(0,1)
# 
# qqplot(ts3,rt(1000,df=58))
# abline(0,1)
# 
# qqplot(ts4,rt(1000,df=58))
# abline(0,1)

ts = replicate(10000,t.test(sample(cad_1m$diff_1d, 30),sample(cad_1m$diff_25d, 30))$p.value)
ts2 = replicate(10000,t.test(sample(cad_3m$diff_1d, 30),sample(cad_3m$diff_25d, 30))$p.value)
ts3 = replicate(10000,t.test(sample(cad_6m$diff_1d, 30),sample(cad_6m$diff_25d, 30))$p.value)
ts4 = replicate(10000,t.test(sample(cad_12m$diff_1d, 30),sample(cad_12m$diff_25d, 30))$p.value)


plot(density(ts))
lines(density(ts2), col='red')
lines(density(ts3), col='blue')
lines(density(ts4), col='green')

# QQ PLOT OF P-VALUE SIMULATIONS
qqnorm(ts); qqline(ts)
qqnorm(rt(10000,df=56)); qqline(rt(10000,df=56))
qqplot(ts,rt(10000,df=56))
abline(0,1)


# T DISTRIBUTIONS 

ts = replicate(1000,t.test(sample(cad_1m$diff_1d, 30),sample(cad_1m$diff_25d, 30))$statistic)
ts2 = replicate(1000,t.test(sample(cad_3m$diff_1d, 30),sample(cad_3m$diff_25d, 30))$statistic)
ts3 = replicate(1000,t.test(sample(cad_6m$diff_1d, 30),sample(cad_6m$diff_25d, 30))$statistic)
ts4 = replicate(1000,t.test(sample(cad_12m$diff_1d, 30),sample(cad_12m$diff_25d, 30))$statistic)

plot(density(ts))
lines(density(ts2), col='red')
lines(density(ts3), col='blue')
lines(density(ts4), col='green')



###### Do this for the 1d_inc and 25_inc in each tenor
t.test(cad_1m$diff_1d[1:30],cad_1m$diff_25d[1:30])
t.test(cad_1m$value[1:30],cad_3m$value[30:60])



################################ MULTIVARIATE ANALYSIS 1D VS 25D ###############################

ellipse_bvn = function(bvn, alpha, colour){
  xbar = apply(bvn,2,mean)
  S = cov(bvn)
  ellipse(xbar, S, alpha = alpha, col=colour, lwd=2)
}


# CAD 1D VS 25D

bvn = data.frame("CAD 1D Increment" = cad_1m$diff_1d[26:5026],"CAD 25D Increment" = cad_1m$diff_25d[26:5026])
bvn2 = data.frame("CAD 1D Increment" = cad_3m$diff_1d[26:5026],"CAD 25D Increment" = cad_3m$diff_25d[26:5026])
bvn3 = data.frame("CAD 1D Increment" = cad_6m$diff_1d[26:5026],"CAD 25D Increment" = cad_6m$diff_25d[26:5026])
bvn4 = data.frame("CAD 1D Increment" = cad_12m$diff_1d[26:5026],"CAD 25D Increment" = cad_12m$diff_25d[26:5026])

cor(bvn)
cor(bvn2)
cor(bvn3)
cor(bvn4)


windows(30, 16)
par(mfrow=c(2,4))


plot(bvn, cex.axis=1.2, cex.lab=1.5, main="CAD 1 Month Tenor")
ellipse_bvn(bvn, .5, colour="green")
ellipse_bvn(bvn, .05, colour="red")
ellipse_bvn(bvn, .01, colour="blue")
legend(2, 2, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.209"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(bvn2, cex.axis=1.2, cex.lab=1.5, main="CAD 3 Month Tenor")
ellipse_bvn(bvn2, .5, colour="green")
ellipse_bvn(bvn2, .05, colour="red")
ellipse_bvn(bvn2, .01, colour="blue")
legend(1.2, 2, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.205"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(bvn3, cex.axis=1.2, cex.lab=1.5, main="CAD 6 Month Tenor")
ellipse_bvn(bvn3, .5, colour="green")
ellipse_bvn(bvn3, .05, colour="red")
ellipse_bvn(bvn3, .01, colour="blue")
legend(0.7, 2, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.203"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(bvn4, cex.axis=1.2, cex.lab=1.5, main="CAD 12 Month Tenor")
ellipse_bvn(bvn4, .5, colour="green")
ellipse_bvn(bvn4, .05, colour="red")
ellipse_bvn(bvn4, .01, colour="blue")
legend(0.4, 2, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.206"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)



# USD 1D VS 25D

bvn5 = data.frame("USD 1D Increment" = usd_1m$diff_1d[26:5026], "USD 25D Increment" = usd_1m$diff_25d[26:5026])
bvn6 = data.frame("USD 1D Increment" = usd_3m$diff_1d[26:5026], "USD 25D Increment" = usd_3m$diff_25d[26:5026])
bvn7 = data.frame("USD 1D Increment" = usd_6m$diff_1d[26:5026], "USD 25D Increment" = usd_6m$diff_25d[26:5026])
bvn8 = data.frame("USD 1D Increment" = usd_12m$diff_1d[26:5026], "USD 25D Increment" = usd_12m$diff_25d[26:5026])

cor(na.omit(bvn5))
cor(na.omit(bvn6))
cor(na.omit(bvn7))
cor(na.omit(bvn8))


plot(bvn5, cex.axis=1.2, cex.lab=1.5, main="USD 1 Month Tenor")
ellipse_bvn(na.omit(bvn5), .5, colour="green")
ellipse_bvn(na.omit(bvn5), .05, colour="red")
ellipse_bvn(na.omit(bvn5), .01, colour="blue")
legend(.6, 2, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.179"),
       col=c("green", "red", "blue", "white"), lty=c(1,1, 1), cex=0.8)

plot(bvn6, cex.axis=1.2, cex.lab=1.5, main="USD 3 Month Tenor")
ellipse_bvn(na.omit(bvn6), .5, colour="green")
ellipse_bvn(na.omit(bvn6), .05, colour="red")
ellipse_bvn(na.omit(bvn6), .01, colour="blue")
legend(.25, 1, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.226"),
       col=c("green", "red", "blue", "white"), lty=c(1,1, 1), cex=0.8)

plot(bvn7, cex.axis=1.2, cex.lab=1.5, main="USD 6 Month Tenor")
ellipse_bvn(na.omit(bvn7), .5, colour="green")
ellipse_bvn(na.omit(bvn7), .05, colour="red")
ellipse_bvn(na.omit(bvn7), .01, colour="blue")
legend(-0.6, 1, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.233"),
       col=c("green", "red", "blue", "white"), lty=c(1,1, 1), cex=0.8)

plot(bvn8, cex.axis=1.2, cex.lab=1.5, main="USD 12 Month Tenor")
ellipse_bvn(na.omit(bvn8), .5, colour="green")
ellipse_bvn(na.omit(bvn8), .05, colour="red")
ellipse_bvn(na.omit(bvn8), .01, colour="blue")
legend(-0.8, 1.1, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.229"),
       col=c("green", "red", "blue", "white"), lty=c(1,1, 1), cex=0.8)


############################# MULTIVARIATE ANALYSIS CAD VS USD ################################



bvn = data.frame("CAD" = cad_1m$diff_1d[26:5026], "USD" = usd_1m$diff_1d[26:5026])
bvn2 = data.frame("CAD" = cad_1m$diff_25d[26:5026], "USD" = usd_1m$diff_25d[26:5026])
bvn3 = data.frame("CAD" = cad_3m$diff_1d[26:5026], "USD" = usd_3m$diff_1d[26:5026])
bvn4 = data.frame("CAD" = cad_3m$diff_25d[26:5026], "USD" = usd_3m$diff_25d[26:5026])
bvn5 = data.frame("CAD" = cad_6m$diff_1d[26:5026], "USD" = usd_6m$diff_1d[26:5026])
bvn6 = data.frame("CAD" = cad_6m$diff_25d[26:5026], "USD" = usd_6m$diff_25d[26:5026])
bvn7 = data.frame("CAD" = cad_12m$diff_1d[26:5026], "USD" = usd_12m$diff_1d[26:5026])
bvn8 = data.frame("CAD" = cad_12m$diff_25d[26:5026], "USD" = usd_12m$diff_25d[26:5026])

cor(na.omit(bvn))
cor(na.omit(bvn2))
cor(na.omit(bvn3))
cor(na.omit(bvn4))


windows(30, 16)
par(mfrow=c(2,4))


plot(na.omit(bvn), cex.axis=1.2, cex.lab=1.5, main="1-Day Increment 1 Month Tenor")
ellipse_bvn(na.omit(bvn), .5, colour="green")
ellipse_bvn(na.omit(bvn), .05, colour="red")
ellipse_bvn(na.omit(bvn), .01, colour="blue")
legend(1.5, 1, legend=c("50% CI", "95% CI", "99% CI", "Corr = -0.021"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(na.omit(bvn2), cex.axis=1.2, cex.lab=1.5, main="25-Day Increment 1 Month Tenor")
ellipse_bvn(na.omit(bvn2), .5, colour="green")
ellipse_bvn(na.omit(bvn2), .05, colour="red")
ellipse_bvn(na.omit(bvn2), .01, colour="blue")
legend(1.8, 2.5, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.015"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(na.omit(bvn3), cex.axis=1.2, cex.lab=1.5, main="1-Day Increment 3 Month Tenor")
ellipse_bvn(na.omit(bvn3), .5, colour="green")
ellipse_bvn(na.omit(bvn3), .05, colour="red")
ellipse_bvn(na.omit(bvn3), .01, colour="blue")
legend(1, 0.4, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.035"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(na.omit(bvn4), cex.axis=1.2, cex.lab=1.5, main="25-Day Increment 3 Month Tenor")
ellipse_bvn(na.omit(bvn4), .5, colour="green")
ellipse_bvn(na.omit(bvn4), .05, colour="red")
ellipse_bvn(na.omit(bvn4), .01, colour="blue")
legend(1.7, 1.5, legend=c("50% CI", "95% CI", "99% CI", "Corr = -0.004"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)


cor(na.omit(bvn5))
cor(na.omit(bvn6))
cor(na.omit(bvn7))
cor(na.omit(bvn8))


plot(na.omit(bvn5), cex.axis=1.2, cex.lab=1.5, main="1-Day Increment 6 Month Tenor")
ellipse_bvn(na.omit(bvn5), .5, colour="green")
ellipse_bvn(na.omit(bvn5), .05, colour="red")
ellipse_bvn(na.omit(bvn5), .01, colour="blue")
legend(.6, 0.3, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.016"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(na.omit(bvn6), cex.axis=1.2, cex.lab=1.5, main="25-Day Increment 6 Month Tenor")
ellipse_bvn(na.omit(bvn6), .5, colour="green")
ellipse_bvn(na.omit(bvn6), .05, colour="red")
ellipse_bvn(na.omit(bvn6), .01, colour="blue")
legend(1.6, 1, legend=c("50% CI", "95% CI", "99% CI", "Corr = -0.014"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(na.omit(bvn7), cex.axis=1.2, cex.lab=1.5, main="1-Day Increment 12 Month Tenor")
ellipse_bvn(na.omit(bvn7), .5, colour="green")
ellipse_bvn(na.omit(bvn7), .05, colour="red")
ellipse_bvn(na.omit(bvn7), .01, colour="blue")
legend(0.4, 0.5, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.033"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)

plot(na.omit(bvn8), cex.axis=1.2, cex.lab=1.5, main="25-Day Increment 12 Month Tenor")
ellipse_bvn(na.omit(bvn8), .5, colour="green")
ellipse_bvn(na.omit(bvn8), .05, colour="red")
ellipse_bvn(na.omit(bvn8), .01, colour="blue")
legend(1.4, 1, legend=c("50% CI", "95% CI", "99% CI", "Corr = 0.016"),
       col=c("green", "red", "blue", "white"), lty=c(1,1,1), cex=0.8)


############################# DISTRIBUTION FITTING ANALYSIS ###################################

## CAD - 1992, 1994, 1995, 1998, 2000-2001, 2007-2009

descdist(cad_1m$diff_1d[2:5834], boot = 1000)


windows(30, 16)
par(mfrow=c(2,2))

#Weird adjustment period in 1992 - 
descdist(cad_1m$diff_1d[601:630], boot = 1000)
legend(0, 7, legend="CAD_1M 1D, Late 1992", cex=0.8)

#1998-1999 adjustments
descdist(cad_1m$diff_1d[2100:2200], boot = 1000)
legend(0, 14, legend="CAD_1M 1D, 1998-1999", cex=0.8)

#Dotcom Bubble 1999-2000
descdist(cad_1m$diff_1d[2400:2500], boot = 1000)
legend(0, 14, legend="CAD_1M 1D, 1999-2000", cex=0.8)

#2008 Recession
descdist(cad_1m$diff_1d[4400:4800], boot = 1000)
legend(3, 4, legend="CAD_1M 1D, 2007-2009", cex=0.8)


cad_1m[4400:4800,]


## USD - 1987-1988, 1991, 2000-2002, 2007-2009

descdist(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)][2:8000], boot = 1000)


windows(30, 16)
par(mfrow=c(2,2))

#1987-1988
descdist(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)][400:700], boot = 1000)
legend(0, 12, legend="USD_1M 1D, 1987-1988", cex=0.8)

#1990-1991
descdist(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)][1200:1500], boot = 1000)
legend(0, 20, legend="USD_1M 1D, 1990-1991", cex=0.8)

#1999-2000
descdist(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)][3600:3800], boot = 1000)
legend(0, 20, legend="USD_1M 1D, 1999-2000", cex=0.8)

#2008
descdist(usd_1m$diff_1d[complete.cases(usd_1m$diff_1d)][5900:6200], boot = 1000)
legend(0, 40, legend="USD_1M 1D, 2008", cex=0.8)



usd_1m[400:700,]
usd_1m[1200:1500,]
usd_1m[2100:2200,]
usd_1m[3600:3800,]
usd_1m[5900:6300,]


############# ??????????????????????????? what to do with z tests and shit #############

sd(cad_3m[complete.cases(cad_3m$value),]$value)
cad_3m[complete.cases(cad_3m$value),]

z <- z.test(cad_1m$value,cad_3m[complete.cases(cad_3m$value),]$value, 
            sigma.x = sd(cad_1m$value), sigma.y = sd(cad_3m[complete.cases(cad_3m$value),]$value))

# sample(cad_1m$value, 10)
# pts = seq(-5,5,length=100)
# plot(pts,dt(pts,df=0.1),col='red',type='l')
# lines(density(ts))
# plot(density(ts))

# probs = c(.9,.95,.99)
# quantile(ts,probs)


########################## DISTRIBUTION FITTING - QQ PLOTS AGAINST GENERATED DISTRIBUTIONS ########################

#qq plot of dataset - 5000 points -
### T DISTRIBUTION BAD

ts_cad1 = sample(cad_1m$diff_1d[complete.cases(cad_1m$diff_1d)], 5000)
plot(density(ts_cad1))

qqnorm(ts_cad1); qqline(ts_cad1)
qqnorm(rt(5000,df=56)); qqline(rt(100,df=56))
qqplot(ts_cad1,rt(5000,df=5000))
abline(0,1)



########################################### MODELLING ##########################################

fx <- expression( theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*x)) ## drift coefficient of model
gx <- expression( theta[3]^2 * (1 - exp(-2*theta[2]*x )) / (2*theta[2]) ) ## diffusion coefficient of model 


usd_1m_1d_sample1 <- as.ts(subset(usd_1m, time>="2000-06-01" & time <= "2004-05-28", select=diff_1d))
usd_1m_25d_sample1 <- as.ts(subset(usd_1m, time>="2000-06-01" & time <= "2004-05-28", select=diff_25d))
usd_1m_value_sample1 <- as.ts(subset(usd_1m, time>="2000-06-01" & time <= "2004-05-28", select=value))

usd_1m_1d_sample2 <- as.ts(subset(usd_1m, time>="2004-05-28" & time <= "2006-07-28", select=diff_1d))
usd_1m_25d_sample2 <- as.ts(subset(usd_1m, time>="2004-05-28" & time <= "2006-07-28", select=diff_25d))
usd_1m_value_sample2 <- as.ts(subset(usd_1m, time>="2004-05-28" & time <= "2006-07-28", select=value))

usd_1m_1d_sample3 <- as.ts(subset(usd_1m, time>="2010-02-13" & time <= "2015-10-10", select=diff_1d))
usd_1m_25d_sample3 <- as.ts(subset(usd_1m, time>="2010-02-13" & time <= "2015-10-10", select=diff_25d))
usd_1m_value_sample3 <- as.ts(subset(usd_1m, time>="2010-02-13" & time <= "2015-10-10", select=value))

#### 1 month tenor sample 1 ####

usd_1d_mod<-fitsde(data = usd_1m_1d_sample1, drift = fx, 
                   diffusion = gx, 
                   start = list(theta1=1, theta2=1, theta3=1), 
                   pmle = "euler")
usd_1d_sim<-(sde.sim(model = "OU", X0 = usd_1m_1d_sample1[1], 
                     theta = as.numeric(coef(usd_1d_mod)), 
                     N = length(usd_1m_1d_sample1), 
                     delta = 1/length(usd_1m_1d_sample1)))
mm <- mean(na.omit(as.numeric(usd_1m_1d_sample1)))
ss <- sd(na.omit(as.numeric(usd_1m_1d_sample1)))
m = median(na.omit(as.numeric(usd_1m_1d_sample1)))
t = mean(abs(na.omit(as.numeric(usd_1m_1d_sample1))-m))
usd_1d_sim_cum_s1 = array(0, dim=c(1))
for (i in 1:length(usd_1m_1d_sample1)){usd_1d_sim_cum_s1[i] <- (cumsum(usd_1d_sim[i]+i*mm) + 
  rlaplace(length(usd_1m_1d_sample1),m,t) + usd_1m_value_sample1[1])}


usd_25d_mod<-fitsde(data = usd_1m_25d_sample1, drift = fx, 
                   diffusion = gx, 
                   start = list(theta1=1, theta2=1, theta3=1), 
                   pmle = "euler")
usd_25d_sim<-(sde.sim(model = "OU", X0 = usd_1m_25d_sample1[1], 
                     theta = as.numeric(coef(usd_25d_mod)), 
                     N = length(usd_1m_25d_sample1), 
                     delta = 1/length(usd_1m_25d_sample1)))
mm <- mean(na.omit(as.numeric(usd_1m_25d_sample1)))/25
ss <- sd(na.omit(as.numeric(usd_1m_25d_sample1)))/25
m = median(na.omit(as.numeric(usd_1m_25d_sample1)))/25
t = mean(abs(na.omit(as.numeric(usd_1m_25d_sample1))-m))/25
usd_25d_sim_cum_s1 = array(0, dim=c(1))
for (i in 1:length(usd_1m_25d_sample1)){usd_25d_sim_cum_s1[i] <- (cumsum(usd_25d_sim[i]+i*mm) + 
  rlaplace(length(usd_1m_25d_sample1),m,t) + usd_1m_value_sample1[1])}



# plots
plot(usd_1m_value_sample1)
lines(usd_1d_sim_cum_s1, type='l',col='red')
lines(usd_25d_sim_cum_s1, type='l',col='blue')

# sma'd
plot(usd_1m_value_sample1)
lines(SMA(usd_1d_sim_cum_s1,n=20), type='l',col='red')
lines(SMA(usd_25d_sim_cum_s1,n=20), type='l',col='blue')





#### 1 month tenor sample 2 ####

usd_1d_mod<-fitsde(data = usd_1m_1d_sample2, drift = fx, 
                   diffusion = gx, 
                   start = list(theta1=1, theta2=1, theta3=1), 
                   pmle = "euler")
usd_1d_sim<-(sde.sim(model = "OU", X0 = usd_1m_1d_sample2[1], 
                     theta = as.numeric(coef(usd_1d_mod)), 
                     N = length(usd_1m_1d_sample2), 
                     delta = 1/length(usd_1m_1d_sample2)))
mm <- mean(na.omit(as.numeric(usd_1m_1d_sample2)))
ss <- sd(na.omit(as.numeric(usd_1m_1d_sample2)))
m = median(na.omit(as.numeric(usd_1m_1d_sample2)))
t = mean(abs(na.omit(as.numeric(usd_1m_1d_sample2))-m))
usd_1d_sim_cum_s2 = array(0, dim=c(1))
for (i in 1:length(usd_1m_1d_sample2)){usd_1d_sim_cum_s2[i] <- cumsum(usd_1d_sim[i]+i*mm) + 
  rlaplace(length(usd_1m_1d_sample2),m,t) + usd_1m_value_sample2[1]}


usd_25d_mod<-fitsde(data = usd_1m_25d_sample2, drift = fx, 
                    diffusion = gx, 
                    start = list(theta1=1, theta2=1, theta3=1), 
                    pmle = "euler")
usd_25d_sim<-(sde.sim(model = "OU", X0 = usd_1m_25d_sample2[1], 
                      theta = as.numeric(coef(usd_25d_mod)), 
                      N = length(usd_1m_25d_sample2), 
                      delta = 1/length(usd_1m_25d_sample2)))
mm <- mean(na.omit(as.numeric(usd_1m_25d_sample2)))/25
ss <- sd(na.omit(as.numeric(usd_1m_25d_sample2)))/25
m = median(na.omit(as.numeric(usd_1m_25d_sample2)))/25
t = mean(abs(na.omit(as.numeric(usd_1m_25d_sample2))-m))/25
usd_25d_sim_cum_s2 = array(0, dim=c(1))
for (i in 1:length(usd_1m_25d_sample2)){usd_25d_sim_cum_s2[i] <- cumsum(usd_25d_sim[i]+i*mm) + 
  rlaplace(length(usd_1m_25d_sample2),m,t) + usd_1m_value_sample2[1]}


# plots
plot(usd_1m_value_sample2)
lines(usd_1d_sim_cum_s2, type='l',col='red')
lines(usd_25d_sim_cum_s2, type='l',col='blue')

# sma'd
plot(usd_1m_value_sample2)
lines(SMA(usd_1d_sim_cum_s2,n=10), type='l',col='red')
lines(SMA(usd_25d_sim_cum_s2,n=10), type='l',col='blue')


#### 1 month tenor sample 3 ####

usd_1d_mod<-fitsde(data = usd_1m_1d_sample3, drift = fx, 
                   diffusion = gx, 
                   start = list(theta1=1, theta2=1, theta3=1), 
                   pmle = "euler")
usd_1d_sim<-(sde.sim(model = "OU", X0 = usd_1m_1d_sample3[1], 
                     theta = as.numeric(coef(usd_1d_mod)), 
                     N = length(usd_1m_1d_sample3), 
                     delta = 1/length(usd_1m_1d_sample3)))
mm <- mean(na.omit(as.numeric(usd_1m_1d_sample3)))
ss <- sd(na.omit(as.numeric(usd_1m_1d_sample3)))
m = median(na.omit(as.numeric(usd_1m_1d_sample3)))
t = mean(abs(na.omit(as.numeric(usd_1m_1d_sample3))-m))
usd_1d_sim_cum_s3 = array(0, dim=c(1))
for (i in 1:length(usd_1m_1d_sample3)){usd_1d_sim_cum_s3[i] <- (cumsum(usd_1d_sim[i]) 
  + usd_1m_value_sample3[1])}


usd_25d_mod<-fitsde(data = usd_1m_25d_sample3, drift = fx, 
                    diffusion = gx, 
                    start = list(theta1=1, theta2=1, theta3=1), 
                    pmle = "euler")
usd_25d_sim<-(sde.sim(model = "OU", X0 = usd_1m_25d_sample3[1], 
                      theta = as.numeric(coef(usd_25d_mod)), 
                      N = length(usd_1m_25d_sample3), 
                      delta = 1/length(usd_1m_25d_sample3)))
mm <- mean(na.omit(as.numeric(usd_1m_25d_sample3)))/25
ss <- sd(na.omit(as.numeric(usd_1m_25d_sample3)))/25
m = median(na.omit(as.numeric(usd_1m_25d_sample3)))/25
t = mean(abs(na.omit(as.numeric(usd_1m_25d_sample3))-m))/25
usd_25d_sim_cum_s3 = array(0, dim=c(1))
for (i in 1:length(usd_1m_25d_sample3)){usd_25d_sim_cum_s3[i] <- (cumsum(usd_25d_sim[i])
    + usd_1m_value_sample3[1])}



# plots
plot(usd_1m_value_sample3, ylim=c(0,0.5))
lines(usd_1d_sim_cum_s3, type='l',col='red')
lines(usd_25d_sim_cum_s3, type='l',col='blue')

# SMA HACK 2019
plot(usd_1m_value_sample3, ylim=c(0,0.5))
lines(SMA(usd_1d_sim_cum_s3,n=30), type='l',col='red')
lines(SMA(usd_25d_sim_cum_s3,n=30), type='l',col='blue')






### test statistics
laplace.test(usd_25d_sim_cum_s1)

fendo.l <- fitdist(as.numeric(usd_25d_sim_cum_s1), "norm")
fendo.ln <- fitdist(as.numeric(usd_25d_sim_cum_s1), "lnorm")
fendo.logis <- fitdist(as.numeric(usd_25d_sim_cum_s1), "logis")
fendo.wei <- fitdist(as.numeric(usd_25d_sim_cum_s1), "weibull")
cdfcomp(list(fendo.l, fendo.ln, fendo.logis, fendo.wei), lwd=2, xlogscale = TRUE,
        ylogscale = TRUE, legendtext = c("norm", "lognorm", "logistic", "weibull"))

gofstat(list(fendo.l, fendo.ln, fendo.logis, fendo.wei),
        fitnames = c("norm", "lognorm", "logistic", "weibull"))




########### TO DO ##############
######### SIMULATE INCREMENT COLUMN AND ADD EM UP USING A STARTING INTEREST RATE VALUE
######### FOR DIFFERENT PERIODS, SAY 1980-1999, 2000-2008, 2009-PRESENT

######### USE SIMULATED BROWNIAN MOTION WITH PCA COMPONENTS, FIT WITH VASICEK MODEL, SEE WHATS GOOD DAWG

# sde package - use maximum likelihood function, estimate 3 parameters from data to use vasicek
# test with different time periods
# theta1 - mean, theta2 - variance, theta3 - 1/tau (tau=time period decay to skip before calculating mean reversion)

# make a model for each separate tenor to get good parameters and nice time series trajectories
# then correlate the separate models (PCA brownian part) 
# (convert independent vasicek noise to dependent using correlation matrix and cholesky decomposition)
#   ^ correlation matrix into upper and lower triangular matrix

# VASICEK THETA3 - autocorrelation coefficient within a time series

# spacial correlation - between tenors - correlation matrix
# temporal correlation - between time periods within a tenor - vasicek model

# fitdistr - ks and anderssondarling

# after proper fitting and simulation >>>
### do monte carlo simulation 1-10 days forward to figure out portfolio price depending in IR
### ex. 1 day monte carlo, generate 100000 points with different interest rates and pricing function
###### generates value-at-risk distribution 
### convexity and duration is second order derivative of pricing function
### can also do taylor approximation giving interest rate

