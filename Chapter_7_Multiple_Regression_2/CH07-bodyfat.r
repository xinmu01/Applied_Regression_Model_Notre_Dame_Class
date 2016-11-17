##################################################################
# the codes demonstrate calculation of extra SS and 
# the multicollinearity issues
##################################################################

setwd("D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code")
bodyfat <- read.table("CH07TA01.txt",header=F,col.names=c("X1","X2","X3","Y"))
attach(bodyfat)
bodyfat
n <- length(Y)


# Fit regression of Y on X1
bodyfat.X1 <- lm(Y ~ X1)    
summary(bodyfat.X1)
anova.X1 <- anova(bodyfat.X1); anova.X1
names(anova.X1)
(SSR_X1 <- anova.X1$Sum[1])   # SSR(X1)
(SSE_X1 <- anova.X1$Sum[2])   # SSE(X1)
(SST <- sum(anova.X1$Sum))    # SST


# Fit regression of Y on X2
bodyfat.X2 <- lm(Y ~ X2)    
summary(bodyfat.X2)
anova.X2 <- anova(bodyfat.X2); anova.X2
(SSR_X2 <- anova.X2$Sum[1])   # SSR(X2)
(SSE_X2 <- anova.X2$Sum[2])   # SSE(X2)


# Fit regression of Y on X1, X2
bodyfat.X1X2 <- lm(Y ~ X1 + X2)    
summary(bodyfat.X1X2)
anova.X12 <- anova(bodyfat.X1X2); anova.X12
(SSR_X12 <- sum(anova.X12$Sum[1:2]))    # SSR(X1,X2)
(SSR_X2.X1 <- anova.X12$Sum[2])         # SSR(X2|X1)
(SSE_X12 <- anova.X12$Sum[3])           # SSE(X1,X2)

# change in the order of X's would not affect the LS results
# but will affect the decomposition of SST.
bodyfat.X2X1 <- lm(Y ~ X2 + X1)    
summary(bodyfat.X2X1)
anova.X21 <- anova(bodyfat.X2X1); anova.X21
(SSR_X12 <- sum(anova.X21$Sum[1:2]))    # SSR(X1,X2)
(SSR_X1.X2 <- anova.X21$Sum[2])         # SSR(X2|X1)
(SSE_X12 <- anova.X21$Sum[3])           # SSE(X1,X2)



# Fit regression of Y on X1, X2, X3
bodyfat.X1X2X3 <- lm(Y ~ X1 + X2 + X3)    
summary(bodyfat.X1X2X3)
anova.X123 <- anova(bodyfat.X1X2X3); anova.X123 
(SSR_X123 <- sum(anova.X123$Sum[1:3]))    # SSR(X1,X2,X3)
(SSR_X3.X12 <- anova.X123$Sum[3])         # SSR(X3|X1,X2)
(SSR_X23.X1 <- sum(anova.X123$Sum[2:3]))  # SSR(X3,X2|X1)
(SSE_X123 <- anova.X123$Sum[4])           # SSE(X1,X2,X3) 

# Coefficient of Partial determination (CPD)
(R2_Y2.1 <- SSR_X2.X1/SSE_X1)        # between Y,X2 given X1
(R2_Y3.12 <- SSR_X3.X12/SSE_X12)     #between Y,X3 given X1,X2
(R2_Y1.2 <- SSR_X1.X2/SSE_X2)        # between Y,X1 given X2



# Coeff of partial correlation
(r_Y2.1 <- sign(coef(bodyfat.X1X2)[3])*sqrt(R2_Y2.1))      # between Y,X2 given X1
(r_Y3.12 <- sign(coef(bodyfat.X1X2X3)[4])*sqrt(R2_Y3.12))  # between Y,X3 given X1,X2
(r_Y1.2 <- sign(coef(bodyfat.X1X2)[2])*sqrt(R2_Y1.2))      # between Y,X1 given X2



# general linear tests; For each hypothes, 
# two differnt codings are provided, either one would work

# F* for H0: beta_3=0; 
(F1<- (SSR_X3.X12/1)/(SSE_X123/(n-4))); 1-pf(F1, 1, n-4)  #1
anova(bodyfat.X1X2, bodyfat.X1X2X3)  #2

# F* for H0: beta_2=beta_3=0 
(F2 <- (SSR_X23.X1/2)/(SSE_X123/(n-4))); 1-pf(F2, 2, n-4)  #1
anova(bodyfat.X1, bodyfat.X1X2X3) #2




