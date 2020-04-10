setwd('D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code')

toluca <- read.table("CH01TA01.txt",header=F)
toluca[1]
toluca[2]
typeof(toluca[1]) ## a list

toluca <- read.table("CH01TA01.txt",header=F,col.names=c("X","Y"))
## toluca is a list (2D), also called dataframe


attach(toluca)

toluca

plot(X,Y) ## X and Y are two column vectors

n <- length(Y)

mean_x <- mean(X); mean_y <- mean(Y)
var_x <- var(X);  var_y <- var(Y);  cov_xy <- cov(X,Y)

SS_xx <- (n-1)*var_x
SS_xy <- (n-1)*cov_xy
SS_yy <- (n-1)*var_y

b1 <- SS_xy/SS_xx
b0 <- mean_y - b1*mean_x ## The fitted line always go through (mean_x,mean_y)

yhat <- b0 + b1*X
e <- Y-yhat ## Residual

SSE <- sum(e^2)
MSE <- SSE/(n-2)
s <- sqrt(MSE)

print(cbind(mean_x,mean_y))
print(cbind(SS_xx,SS_xy,SS_yy))
print(cbind(b0,b1))
print(cbind(Y,yhat,e))

plot(X,Y,xlim=c(0,125))
abline(a=b0,b=b1, col='red')  

