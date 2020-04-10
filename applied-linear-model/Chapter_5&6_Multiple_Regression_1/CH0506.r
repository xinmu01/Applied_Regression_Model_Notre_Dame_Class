##############################################
# practice matrix operation in R
# the tuloca company example from chap 1
##############################################

setwd("D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code")
toluca <- read.table("CH01TA01.txt",header=F,col.names=c("X","Y"))
attach(toluca)

n <- length(Y)

X0 <- rep(1,n)
X <- as.matrix(cbind(X0,X))    # Form the X-matrix (n=25 rows, 2 Cols)
Y <- as.matrix(Y,ncol=1)       # Form the Y-vector (n=25 rows, 1 col)

# t(X):  transpose of X, 
# %*% : matrix multiplication,
# solve(A):  inverse of A

(XX <- t(X) %*% X)             # Obtain X'X matrix (2 rows, 2 cols)
(XY <- t(X) %*% Y)             # Obtain X'Y vector (2 rows, 1 col)
(XXI <- solve(XX))             # Obtain (X'X)^(-1) matrix (2 rows, 2 cols)


(b <- XXI %*% XY)              # Obtain b-vector (2 rows, 1 col)
Y_hat <- X %*% b               # Obtain the vector of fitted values (25 rows, 1 col)
e <- Y - Y_hat                 # Obtain the vector of residuals (25 rows, 1 col)
print(cbind(Y_hat,e))


H <- X %*% XXI %*% t(X)              # Obtain the Hat matrix
J_n <- matrix(rep(1/n,n^2),ncol=n)   # Obtain the (1/n)J matrix (25 rows, 25 cols)   
I_n <- diag(n)                       # Obtain the identity matrix (25 rows, 25 cols)  


(SST <- t(Y) %*% (I_n - J_n) %*% Y)  # Obtain Total Sum of Squares (SST) 
(SSE <- t(Y) %*% (I_n - H) %*% Y)    # Obtain Error Sum of Squares (SSE)             
(SSR <- t(Y) %*% (H - J_n) %*% Y)    # Obtain Regression Sum of Squares (SSR)             


(MSE <- SSE/(n-2))                   # Obtain MSE 
is.matrix(MSE) 
dim(MSE)
(s2_b <- MSE[1,1] * XXI)             # MSE is a matrix rather than a sclar; use MSE[1,1]


(X_h <- matrix(c(1,65),ncol=1))      # Create X_h vector, for case where lot size=65
(Y_hat_h <- t(X_h) %*% b)            # Obtain the fitted value when lot size=65
(s2_yhat_h <- t(X_h) %*% s2_b %*% X_h)        # Obtain s^2{Y_hat_h}
(s2_pred <- MSE + (t(X_h) %*% s2_b %*% X_h))  # Obtain s^2{pred}

detach(toluca)


##############################################
# case study in Chap 6
##############################################
rm(list=ls())
dwaine <- read.table("dwaine_studios.txt",header=TRUE)
dwaine <- dwaine[,-1] # Delete first column
attach(dwaine)
(n <- nrow(dwaine))

plot(dwaine)  #scatter plot
cor(dwaine)


dwaine.reg1 <- lm(Y~X1+X2)
(summ <-summary(dwaine.reg1)) # can get R2, adjusted R2, F-test of beta1=beta2=0
summ$coeff   # LS estimates of beta's, std errs and t tests of whether each of them =0
summ$fstat   # global F-test
summ$r.      # Coefficient of determination (CD)
summ$adj.r   # Adjusted CD
summ$sigma   # estimate of sigma
summ$cov     # estimate of the variance-convariance structure of Beta (p x p)

r<- rstudent(dwaine.reg1)   # studentized residuals
plot(r,predict(dwaine.reg1 ))
plot(X1, r)
plot(X2, r)
qqnorm(r)
qqline(r, col='red')
plot(X1*X2, r)
# check on the intx term
dwaine.reg2 <- lm(Y~X1+X2+X1*X2)
summary(dwaine.reg2)


# simulation 90% CI of b via Bonferron's
coef <- summ$coef         
df_E  <- summ$df[2]

(b1_95CI <- c(coef[2,1]-qt(.975,df_E)*coef[2,2], 
              coef[2,1]+qt(.975,df_E)*coef[2,2]))         
(b2_95CI <- c(coef[3,1]-qt(.975,df_E)*coef[3,2],
              coef[3,1]+qt(.975,df_E)*coef[3,2])) 


# mean response estimation
xh <- data.frame(X1=65.4,X2=17.6)  # Set up X_h vector 
predict(dwaine.reg1, newdata=xh, se.fit=T, interval='confidence')


# predict two new y's via Bonferroni's 
xh <- data.frame(X1=c(65.4,53.1),X2=(c(17.6, 17.7))) # Set up X_h vector
# Bonferroni
(pred<- predict(dwaine.reg1, newdata=xh, se.fit=T, interval='prediction', level=0.95))            
# Scheffe (90% PI)
(S_90PI_A <- c(pred$fit[1,1]-sqrt(2*qf(.90,2,pred$df))*sqrt((pred$se[1])^2+summ$sigma^2),
               pred$fit[1,1]+sqrt(2*qf(.90,2,pred$df))*sqrt((pred$se[1])^2+summ$sigma^2))) 
(S_90PI_B <- c(pred$fit[2,1]-sqrt(2*qf(.90,2,pred$df))*sqrt((pred$se[2])^2+summ$sigma^2),
               pred$fit[2,1]+sqrt(2*qf(.90,2,pred$df))*sqrt((pred$se[2])^2+summ$sigma^2)))
