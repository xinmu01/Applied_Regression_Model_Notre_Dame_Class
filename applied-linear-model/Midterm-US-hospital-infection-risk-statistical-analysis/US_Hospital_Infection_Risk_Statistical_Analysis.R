
#In this project, I explore a hospital infection rate dataset, and build a linear regression model to predict the 
#infection rate.

rm(list=ls())
setwd("C:\\Users\\Xin\\Desktop\\Xin_Mu_Study\\Git-repo\\git-fast\\data-science-projects\\6-US-hospital-infection-risk-statistical-analysis\\data")
hospital <- read.table("US_Hospital_Infection_Risk_Statistical_Analysis.txt", header = FALSE, col.names = c("ID","X_LS","X_Age","Y",
                                                                             "X_Rcul","X_RcXray","X_beds",
                                                                             "X_MedScho","X_Regi","X_census",
  
                                                                                                                                                        "X_nurse","X_serv"))
attach(hospital)

##Examine relation between Y and all the predictors
plot(hospital)
cor(hospital)

## Dummy coding X_MedScho variable
X_MedScho <- factor(X_MedScho)
X_Regi <- factor(X_Regi)
is.factor(X_MedScho)
is.factor(X_Regi)

## Fit a linear model on all the variables
hospital.reg <- lm(Y~X_LS+X_Age+X_Rcul+X_RcXray+X_beds+X_MedScho+X_Regi+X_census+X_nurse+X_serv)
summary(hospital.reg)

## ANOVA 
anova(hospital.reg)

## Model diagnostics
r <- rstudent(hospital.reg)

par(mfrow=c(3,3))
par(mar=c(4,4,2,2))

plot (predict(hospital.reg),r,xlab = "Y_hat")
plot (X_LS,r)
plot (X_Rcul,r)
plot(X_Age,r)
plot(X_beds,r)
plot(X_census,r)
plot(X_serv,r)
plot(X_nurse,r)
plot(X_RcXray,r)

qqnorm(r)
qqline(r, col='red')

## Make predictions, mean response and predict new is the same for the value
xh <- data.frame(X_LS=10,X_Age=50,X_Rcul=15,X_RcXray=100,X_beds=300,X_MedScho=1,X_Regi=2,X_census=250,X_nurse=200,X_serv=40)
xh$X_MedScho <- factor(xh$X_MedScho)
xh$X_Regi <- factor(xh$X_Regi)

## Estimate of mean response
predict(hospital.reg, newdata=xh, se.fit=T, interval='confidence',level = 0.95)
## Estimate of prediction of new example
predict(hospital.reg, newdata=xh, se.fit=T, interval='prediction',level = 0.95)

## Linear fit with some interaction terms
hospital.reg1 <- lm(Y~X_LS+X_Age+X_Rcul+X_RcXray+X_beds+X_MedScho+X_Regi+X_census+X_nurse+X_serv+X_RcXray*X_beds+X_LS*X_Rcul)
summary(hospital.reg1)

## Make prediction using new model
xh1 <- data.frame(X_LS=10,X_Age=50,X_Rcul=15,X_RcXray=100,X_beds=300,X_MedScho=1,X_Regi=2,X_census=250,X_nurse=200,X_serv=40,X_RcXray_X_beds = 30000, X_LS_X_Rcul=150)
xh1$X_MedScho <- factor(xh1$X_MedScho)
xh1$X_Regi <- factor(xh1$X_Regi)

## Estimate of mean response
predict(hospital.reg1, newdata=xh1, se.fit=T, interval='confidence',level = 0.95)
## Estimate of prediction of new example
predict(hospital.reg1, newdata=xh1, se.fit=T, interval='prediction',level = 0.95)


