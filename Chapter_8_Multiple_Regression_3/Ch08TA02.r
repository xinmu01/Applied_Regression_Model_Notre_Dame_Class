############## the insurance innovation example #############

setwd("D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code")

ins_innov <- read.table("CH08TA02.txt", header=F,col.names=c("Y","X1","X2"))

attach(ins_innov)

############  Fit main effects model   

ins.reg1 <- lm(Y ~ X1 + X2)
summary(ins.reg1)
anova(ins.reg1)

x1 <- seq(0,310,1)          
yhat_m <- coef(ins.reg1)[1] + coef(ins.reg1)[2]*x1
yhat_s <- coef(ins.reg1)[1] + coef(ins.reg1)[2]*x1 + coef(ins.reg1)[3]

plot(x1,yhat_s,xlim=c(0,310),type="l",lty=1,main="Additive Model")
lines(x1,yhat_m,type="l",lty=2, col='red')
points(X1[X2==1],Y[X2==1],pch=1)
points(X1[X2==0],Y[X2==0],pch=2, col='red')


############  Fit interaction model   

ins.reg2 <- lm(Y ~ X1 + X2 + I(X1*X2))
summary(ins.reg2)
anova(ins.reg2)
drop1(ins.reg2)

x1 <- seq(0,310,1)
yhat_m <- coef(ins.reg2)[1] + coef(ins.reg2)[2]*x1
yhat_s <- coef(ins.reg2)[1] + coef(ins.reg2)[2]*x1 + coef(ins.reg2)[3] + coef(ins.reg2)[4]*x1

plot(x1,yhat_s,xlim=c(0,310),type="l",lty=1,main="Interaction Model")
lines(x1,yhat_m,type="l",lty=2, col='red')
points(X1[X2==1],Y[X2==1],pch=1)
points(X1[X2==0],Y[X2==0],pch=2, col='red')


##############  Compare Models (F-test equivalent to t-test for beta1)

anova(ins.reg1,ins.reg2)

