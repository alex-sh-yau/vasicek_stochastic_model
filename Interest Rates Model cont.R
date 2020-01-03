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
library(LaplacesDemon)
library(vsgoftest)


install.packages("vsgoftest")



cad_1m = read.csv(".\\Project\\CAD1MTD156N.csv")
cad_3m = read.csv(".\\Project\\CAD3MTD156N.csv")
cad_6m = read.csv(".\\Project\\CAD6MTD156N.csv")
cad_12m = read.csv(".\\Project\\CAD12MD156N.csv")

usd_1m = read.csv(".\\Project\\USD1MTD156N.csv")
usd_3m = read.csv(".\\Project\\USD3MTD156N.csv")
usd_6m = read.csv(".\\Project\\USD6MTD156N.csv")
usd_12m = read.csv(".\\Project\\USD12MD156N.csv")




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


########################################### DIFFERENCE CALCULATIONS ############################################ 

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


################################################## scrap ################################################### 


sample <- subset(cad_1m, time>="2010-09-07" & time <= "2011-09-09", select=value)
sample["tenor"] <- (1/12)
sample <- as.ts(sample)


sample2 <- subset(cad_3m, time>="2010-09-07" & time <= "2011-09-09", select=value)
sample2["tenor"] <- (3/12)
sample2 <- as.ts(sample2)


sample3 <- subset(cad_6m, time>="2010-09-07" & time <= "2011-09-09", select=value)
sample3["tenor"] <- (6/12)
sample3 <- as.ts(sample3)


sample4 <- subset(cad_12m, time>="2010-09-07" & time <= "2011-09-09", select=value)
sample4["tenor"] <- (1/5.44)
sample4 <- as.ts(sample4)
#########



# fitmod <- fitsde(data = sample, drift = fx, diffusion = gx, start = list(theta1=1,
#                                                                          theta2=1,
#                                                                          theta3=1,
#                                                                          theta4=1),pmle="euler")
# coef(fitmod)

# fx <- expression( theta[1]+theta[2]*x ) ## drift coefficient of model
# gx <- expression( theta[3]*x^theta[4] ) ## diffusion coefficient of model 
fx <- expression( theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*x)) ## drift coefficient of model
gx <- expression( theta[3]^2 * (1 - exp(-2*theta[2]*x )) / (2*theta[2]) ) ## diffusion coefficient of model 


fitsde(data = sample3, drift = fx, diffusion = gx, start = list(theta1 = 1, 
                                                               theta2 = 1, 
                                                               theta3 = 1), pmle = "euler")

get.vasicek.param(sample, 1)
get.vasicek.param(sample2, 1)
get.vasicek.param(sample3, 1)
get.vasicek.param(sample4, 1)


est.vasicek(data = sample, method = "Hessian", days = 250, significanceLevel = 0.95)
lines(sample, col='red')

est.vasicek(data = sample2, method = "Hessian", days = 250, significanceLevel = 0.95)
lines(sample2, col='red')

est.vasicek(data = sample3, method = "Hessian", days = 250, significanceLevel = 0.95)
lines(sample3, col='red')

big <- est.vasicek(data = sample4, method = "Hessian", days = 250, significanceLevel = 0.95)
plot(big$r, type="l", ylim=c(1.5,3))
lines(sample4$value, col='red')


# trial=as.numeric(na.omit(cad_1m[,2]))
# #trial=ts(cad_1m_diff_1[,1])
# X<-trial
# X
# #X<-tester$residuals
# plot(X)
# plot(na.omit(cad_1m[,3]))
# mean(X)
# sd(X)
# 
# X <- as.ts(X)


#####

sample <- subset(usd_1m, time>="2005-09-07" & time <= "2006-09-09", select=diff_1d) 
sample <- as.ts(sample)

sample2 <- subset(usd_3m, time>="2005-09-07" & time <= "2006-09-09", select=diff_1d) 
sample2 <- as.ts(sample2)

sample3 <- subset(usd_6m, time>="2005-09-07" & time <= "2006-09-09", select=diff_1d) 
sample3 <- as.ts(sample3)

sample4 <- subset(usd_12m, time>="2005-09-07" & time <= "2006-09-09", select=diff_1d)
sample4 <- as.ts(sample4)


################################################## SIMPLE MODEL ################################################### 



# fx <- expression( theta[1]+theta[2]*x ) ## drift coefficient of model
# gx <- expression( theta[3]*x^theta[4] ) ## diffusion coefficient of model

fx <- expression( theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*x)) ## drift coefficient of model
gx <- expression( theta[3]^2 * (1 - exp(-2*theta[2]*x )) / (2*theta[2]) ) ## diffusion coefficient of model

sample <- subset(usd_1m, time>="2000-06-01" & time <= "2004-05-28", select=diff_25d) 
sample <- as.ts(sample)

sample <- subset(usd_1m, time>="2004-05-28" & time <= "2006-07-28", select=diff_1d) 
sample <- as.ts(sample)

sample <- subset(usd_1m, time>="2010-02-13" & time <= "2015-10-10", select=diff_25d) 
sample <- as.ts(sample)
sample

# fx <- expression( x0*exp(-theta[1]*x) + theta[2]*(1-exp(-theta[1]*x))) ## drift coefficient of model
# gx <- expression( theta[3]^2 * (1 - exp(-2*theta[2]*x )) / (2*theta[2]) ) ## diffusion coefficient of model

fitmod<-fitsde(data = sample, drift = fx, diffusion = gx, start = list(theta1 = 1, 
                                                                theta2 = 1, 
                                                                theta3 = 1), pmle = "euler")


z<-(sde.sim ( model = "OU" ,X0=0.465, theta =as.numeric(coef(fitmod)), N =1042 , delta =1/1042))
plot(z)
plot(sample, col='red')
mean(z)
sd(z)



sample3v <- subset(usd_1m, time>="2000-06-01" & time <= "2004-05-28", select=value) 
sample3v <- as.ts(sample3v)
plot(sample3v, col='red')

# descdist(as.numeric(z), boot=1000)
z
cumsum(z)
zzz = array(0, dim=c(1))

for (i in 1:1042){zzz[i] <-  + cumsum(z[i]-i*(0.129082/25)+i^(0.25046/25)) + 6.66}
plot(zzz, type='l')
for (i in 1:1042){zzz[i] <- cumsum(z[i]-i*(0.129082/25)) + rlaplace(1415,m,t) + 6.66}
plot(zzz, type='l')

mean(na.omit(as.numeric(sample)))
sd(na.omit(as.numeric(sample)))

m = median(na.omit(as.numeric(sample)))
t = mean(abs(na.omit(as.numeric(sample))-m))

median(z)
mean(abs(z)-median(z))

for (i in 1:250){k[i]<-(cumsum(z[i]+i*mean(X)+rlaplace(250,m,t)))}
m = median(cad_1m_diff_1[,2])
t = mean(abs(cad_1m_diff_1[,2]-m))


z
cumsum(z)
plot(cumsum(z)+1.09, type='l')


##testing 
# eps <- rnorm(1000,0,1)
# n<-300
# drift <- 1
# x1    <- rep(0, n)
# for(i in seq.int(2, n)){
#   x1[i] <- drift + x1[i-1] + eps[i]
# }
# plot(ts(x1))


###############



##9-Vasicek

dcOU <- function (x , t , x0 , theta , log = FALSE ){
  Ex <- theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*t)
  Vx <- theta[3]^2 * (1 - exp(-2*theta[2]*t )) / (2*theta[2])
  dnorm (x , mean=Ex , sd = sqrt(Vx), log = log )
}

OU.lik <- function ( theta1 , theta2 , theta3 ){
  n <- length(X)
  dt <- deltat(X)
  -sum(dcOU(X[2:n],t=dt,x0=X[1:( n-1)] , c ( theta1 , theta2 , theta3 ) , log = TRUE ))
}

#set.seed(123)
#X <- sde.sim ( model = "OU" , theta = c(3 ,1 ,2) , N =5000 , delta =1)
trial=as.numeric(na.omit(cad_1m[,2]))
#trial=ts(cad_1m_diff_1[,1])
X<-trial
X
#X<-tester$residuals
plot(X)
plot(na.omit(cad_1m[,3]))
mean(X)
sd(X)
fit <- mle ( OU.lik , start = list ( theta1 =0.004 , theta2 = 1 , theta3 =0.05) ,method ="L-BFGS-B" , lower = c( -Inf ,-Inf ,0.000001), upper=c(Inf,Inf,Inf))
summary (fit)
confint(fit)



X <- as.ts(X)


z<-(sde.sim ( model = "OU" ,X0=0.947, theta = c(0.1352917 ,0.409908 ,2.157533) , N =255 , delta =1/255))
z
plot(z)
mean(z)
sd(z)

#c(-2.9285990, -3.2679900, -2.3335730)

zz=diff(z,lag=1)
laplace.test(zz)
zz<-(sde.sim ( model = "OU" ,X0=0.01, theta = c(2.359725 ,0.3886281 ,0.3598239) , N =255 , delta =1/255))
zz
plot(zz)
mean(zz)
sd(zz)

##The estimated thetas are statistically insignificant, reduce time duration of the original libor data, or choose a more smooth regime



A = matrix(c(1, 0.8, 0.5, 0.8, 1, 0.8, 0.5, 0.8, 1), nrow=3)
##A comes from actual correlation matrix of original data 12 by 12 package Hmisc
A 
L= t(chol(A))
L
L%*%t(L)
#U= array(12,1000)
#for (i in 1:12) U[i,]= rnorm(1000,0,1)
U=array(0, c(5201,12))
for (i in 1:12) U[,i]=sde.sim ( model = "OU" ,X0=0.05, theta =c(0.002828937 ,0.001215810 ,0.066788878) , N =5200 , delta =1/5200)
U  
#zz[j]

U=t(U)
cor(t(U),t(U))
cor_version_ofU=L%*%U
cor_version_ofU
j=t(cor_version_ofU)

for (i in 1:12)
{windows(20,10)
  plot(j[,i])}

##Below, Checking the computation was right:
cor(t(cor_version_ofU),t(cor_version_ofU))


## instead of the rnorm, use the ditr and parameters that represent each tenor


