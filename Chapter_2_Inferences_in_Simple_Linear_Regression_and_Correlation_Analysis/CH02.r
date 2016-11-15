setwd('D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code')
toluca <- read.table("CH01TA01.txt",header=F,col.names=c("X","Y"))

#############
attach(toluca)

toluca

plot(X,Y)

n <- length(Y)

mean_x <- mean(X); mean_y <- mean(Y)
var_x <- var(X);  var_y <- var(Y);  cov_xy <- cov(X,Y)

SS_xx <- (n-1)*var_x
SS_xy <- (n-1)*cov_xy
SS_yy <- (n-1)*var_y

b1 <- SS_xy/SS_xx
b0 <- mean_y - b1*mean_x

yhat <- b0 + b1*X
e <- Y-yhat

SSE <- sum(e^2)
MSE <- SSE/(n-2)
s <- sqrt(MSE)

################## the above codes are the same as  in Chap 1 ##############

# standard errors of b0 and b1
s_b1 <- s/sqrt(SS_xx)
s_b0 <- s*sqrt((1/n)+(mean_x^2/SS_xx))

# caculate the test staistics for testing the hypotheses
# H0: beta1 = 0 vs. H1: beta1 != 0
t_b1 <- b1/s_b1
# H0: beta0 = 0 vs. H1: beta0 != 0
t_b0 <- b0/s_b0

# calculate the 2-sided p-value
p_b1 <- 2*(1-pt(abs(t_b1),n-2))
p_b0 <- 2*(1-pt(abs(t_b0),n-2))

# calculate the 95% CIs for beta0 and beta1
lb_b1_95 <- b1 - qt(.975,n-2)*s_b1; ub_b1_95 <- b1 + qt(.975,n-2)*s_b1;
lb_b0_95 <- b0 - qt(.975,n-2)*s_b0; ub_b0_95 <- b0 + qt(.975,n-2)*s_b0;

## Remember pt() and qt() commands

print(cbind(b0,s_b0,t_b0,p_b0,lb_b0_95,ub_b0_95))
print(cbind(b1,s_b1,t_b1,p_b1,lb_b1_95,ub_b1_95))

# the one-line code the does everything we did in Chap and the above
toluca.reg1 <- lm(Y ~ X)
summary(toluca.reg1)
confint(toluca.reg1) # obtain the inferences of b0 and b1, this gives the 95% CI

#estimation of E(Y) and prediction of Y
xh = data.frame(X=105) # this data structure is required for predict here
predict(toluca.reg1, xh, interval = "confidence")
predict(toluca.reg1, xh, interval = "prediction")

#xh1 = list(105)
#predict(toluca.reg1, xh1, interval = "prediction")

# confidence band
plot(X,Y,xlim=c(20,120),ylim=c(0,600),main="Toluca Data - Confidence Bands and Loess")
abline(a=b0,b=b1)  

Xh <- 20:120
lines(Xh,b0+b1*Xh - sqrt(2*qf(.95,2,n-2))*s*sqrt((1/n)+((Xh-mean_x)^2/SS_xx)), col='red')
lines(Xh,b0+b1*Xh + sqrt(2*qf(.95,2,n-2))*s*sqrt((1/n)+((Xh-mean_x)^2/SS_xx)), col='red')

lines(Xh,b0+b1*Xh - qt(.975,n-2)*s*sqrt((1/n)+((Xh-mean_x)^2/SS_xx)), col='blue')
lines(Xh,b0+b1*Xh + qt(.975,n-2)*s*sqrt((1/n)+((Xh-mean_x)^2/SS_xx)), col='blue')



#####################################################
# Pearson correlation analysis 
#####################################################

hist(X)
hist(Y)
r=cor(X,Y) 
r

# test procedure 1
t <- r*sqrt(n-2)/sqrt(1-r^2)
p_r <- 2*(1-pt(abs(t),n-2))
p_r


# test procedure 2 (based on Fisher's z transformation)
z= 1/2*log((1+r)/(1-r))
z.test = z*sqrt(n-3)
p_r <- 2*(1-pnorm(abs(z.test)))
p_r
CI.z = c(z-qnorm(0.975)/sqrt(n-3), z+qnorm(0.975)/sqrt(n-3))
(exp(2*CI.z)-1)/(exp(2*CI.z)+1)


cor.test(X,Y,method="pearson",conf.int=TRUE)
# the confidence interval is based on Fisher z Transformation

# clean up everything
detach(toluca)
rm(list=ls())

#####################################################
# Spearman correlation analysis (Example Table 2.4) 
#####################################################

markres <- read.table("CH02TA04.txt",header=F,col.names=c("Y1","Y2"))
attach(markres)
hist(Y1)
hist(Y2)
R1 <- rank(Y1)
R2 <- rank(Y2)

n <- length(Y1)

r_S <- cor(R1,R2)
r_S

t_rS <- r_S*sqrt(n-2)/sqrt(1-r_S^2)
p_rS <- 2*(1-pt(abs(t_rS),n-2))
p_rS

cor.test(Y1,Y2,method="spearman") # cor.test do not calculate CI for spearman r
detach(markres)

