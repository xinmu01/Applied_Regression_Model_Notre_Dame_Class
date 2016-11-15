#################### residual analysis #################### 

setwd("D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code")

mapdist <- read.table("CH03TA01.txt", header=F,col.names=c("Y","X"))
attach(mapdist)

mapdist.reg1 <- lm(Y ~ X)
summary(mapdist.reg1)
plot(X,Y,xlim=c(60,260),ylim=c(0,10),main="Y vs X and Fitted equation")
abline(a=mapdist.reg1$coef[1],b=mapdist.reg1$coef[2])

# resdiual
r <- resid(mapdist.reg1)
# resdiual
rr <- rstandard(mapdist.reg1) ## This is semi-studentized or say standardized residuals
# resdiual
rrr <- rstudent(mapdist.reg1)
cbind(r,rr,rrr)

# constant variance; linearity
plot(X,rr,xlab="Maps Distributed",ylab="Residual",main="Residuals vs X")
abline(h=0)

# provides the same info as the above
plot(predict(mapdist.reg1),rr,xlab="Fitted Y",ylab="Residual",main="Residuals vs Y_hat")
abline(h=0)

# assume the X's are collected temporally, we can get autocorrelation among the 
# residual by 
my.acf = acf(rr)
names(my.acf)
my.acf$acf

detach(mapdist)


########### another example of residual analysis #########

plasma <- read.table("CH03TA08.txt",header=F,col.names=c("X","Y","log10Y"))
attach(plasma)

plot(X,Y)

plasma.reg1 <- lm(Y ~ X)
summary(plasma.reg1)
rr<- rstandard(plasma.reg1)


plot(X,rr,main="Residuals vs X") # constant variance; linearity
hist(rr)  # normality, outlying observations
qqnorm(rr,main="Residuals Normal Probability Plot") # normality
qqline(rr)

detach(plasma)


#################### Lack of Fit (F test) ###################

bank <- read.table("CH03TA04.txt", header=F,col.names=c("X","Y"))
attach(bank)
plot(X,Y)

# reduced model
m1 <- lm(Y~X)
summary(m1)

# full model
m2 <- lm(Y~factor(X))
summary(m2)

# Lack-of-Fit test
anova(m1, m2)

detach(bank)


########## transformation of X ###########

saletrn <- read.table("CH03TA07.txt", header=F,col.names=c("X","Y"))
attach(saletrn)

plot(X,Y)

saletrn.reg1 <- lm(Y~X)
summary(saletrn.reg1)
rr= rstandard(saletrn.reg1)
plot(predict(saletrn.reg1), rr)
qqnorm(rr,main="Residuals Normal Probability Plot") # normality
qqline(rr)

sqrtX <- sqrt(X)
plot(sqrtX,Y)
saletrn.reg2 <- lm(Y~sqrtX)
summary(saletrn.reg2)
rr= rstandard(saletrn.reg2)
plot(predict(saletrn.reg2), rr)
qqnorm(rr,main="Residuals Normal Probability Plot") # normality
qqline(rr)

detach(saletrn)

########## transformation of Y ###########

plasma <- read.table("CH03TA08.txt",header=F,col.names=c("X","Y","log10Y"))
attach(plasma)

plot(X,Y)
plot(X,log10Y)

plasma.reg2 <- lm(log10Y ~ X)
summary(plasma.reg2)
rr<- rstandard(plasma.reg2)


plot(X,rr,main="Residuals vs X") # constant variance; linearity
hist(rr)  # normality, outlying observations
qqnorm(rr,main="Residuals Normal Probability Plot") # normality
qqline(rr)



############## Box-Cox Transformation (must load MASS library first)

library(MASS)
plasma.reg2 <- lm(Y~X)
bc= boxcox(plasma.reg2,plotit=T)
lambda = bc$x[order(bc$y, decreasing = TRUE)[1]]
lambda
plasma.reg3 <- lm(Y^lambda~X)
summary(plasma.reg3)
rr<- rstandard(plasma.reg3)


plot(X,rr,main="Residuals vs X") # constant variance; linearity
hist(rr)  # normality, outlying observations
qqnorm(rr,main="Residuals Normal Probability Plot") # normality
qqline(rr)
detach(plasma)



################### case study #################
# Dataset:  cotton_spindle.dat
# Source: T. Leung (2003), "A British Industrial Success: Productivity in the
# Lancashire and New England Cotton Spinning Industries a Century Ago," 
# Economic History Review, LVI, 1, pp. 90-117.

# Description: Relationship between yarn count (low=coarse, high=fine)
# and output for New England Cotton spindles of types mule spinning and ring
# spinning. They fit models with y=output, x=1/count

# Variables
# Spinning type (1=Mule, 2=Ring)
# Yarn count         
# Output (lbs/week)  

spindle <- read.table("cotton_spindle.dat",header=F, col.names=c("type","yarncount","output"))

type1= spindle[spindle$type==1,] # we only work with type1
attach(type1)

plot( yarncount, output)


spindle.reg <- lm(output ~ yarncount)
summary(spindle.reg)

spindle.aov <- lm(output~ factor(yarncount))
summary(spindle.aov)

anova(spindle.reg,spindle.aov)


### Y ~ 1/X
plot(1/yarncount, output)
xinv = 1/yarncount
X.inv <- lm(output ~ xinv)
summary(X.inv)
plot(X.inv)


# BC transformation
library(MASS)
boxcox(spindle.reg,plotit=T)
bc= boxcox(spindle.reg,lambda=seq(-1,0,0.01),plotit=T)
lambda = bc$x[order(bc$y, decreasing = TRUE)[1]]

newy= output^lamdba
Y.BC <- lm(newy~ yarncount)
summary(Y.BC)
plot(Y.BC)


detach(plasma)


