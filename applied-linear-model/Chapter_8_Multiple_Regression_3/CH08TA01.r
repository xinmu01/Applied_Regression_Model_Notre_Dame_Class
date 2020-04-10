########### the power cell example ###############

setwd("D:\\XinMu\\Course_Work\\Fall_2016\\Linear Model\\R_code")

power_cells <- read.table("CH08TA01.txt",header=F, col.names=c("cycles","chrg","temp"))

attach(power_cells)

chrg_c <- (chrg-mean(chrg))/0.4; chrg_c2 <- chrg_c^2
temp_c <- (temp-mean(temp))/10;  temp_c2 <- temp_c^2
chrgtemp_c <- chrg_c*temp_c


# model with full 2nd-order terms on centered x's
pc.mod1 <- lm(cycles ~ chrg_c + temp_c + chrg_c2 + temp_c2 + chrgtemp_c)
summary(pc.mod1)
anova(pc.mod1)


# model with main effects only on centered x's
pc.mod2 <- lm(cycles ~ chrg_c + temp_c)
summary(pc.mod2)
anova(pc.mod2)

anova(pc.mod2, pc.mod1)



#####  Run model on raw predictors for plots   ##############

pc.1_nc <- lm(cycles ~ chrg + temp + I(chrg^2) + I(temp^2) + I(chrg*temp))
summary(pc.1_nc)

pc.2_nc <- lm(cycles ~ chrg + temp)
summary(pc.2_nc)


