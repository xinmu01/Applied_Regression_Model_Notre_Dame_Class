##################################################################
# the codes demonstrate the application of standardized regression
##################################################################
setwd("D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code")

dwaine <- read.table("dwaine_studios.txt",header=F,col.names=c("City","Y","X1","X2"))
attach(dwaine)
dwaine
n <- length(Y)


# Correlation transformation
Y_corr <- (1/sqrt(n-1))*(Y-mean(Y))/sd(Y)        
X1_corr <- (1/sqrt(n-1))*(X1-mean(X1))/sd(X1)   
X2_corr <- (1/sqrt(n-1))*(X2-mean(X2))/sd(X2) 
print(cbind(Y_corr,X1_corr,X2_corr))


# Regression of Y* on X1*,X2*, no intercept
dwaine.stdreg <- lm(Y_corr ~ X1_corr + X2_corr -1)  
summary(dwaine.stdreg)


(b1 <- (sd(Y)/sd(X1))*coef(dwaine.stdreg)[1])   # Compute b1 from b1*
(b2 <- (sd(Y)/sd(X2))*coef(dwaine.stdreg)[2])   # Compute b2 from b2*
(b0 <- mean(Y) - b1*mean(X1) - b2*mean(X2))     # Comute b0 


# Regression of Y on X1,X2
dwaine.reg <- lm(Y ~ X1 + X2)                        
summary(dwaine.reg)
