load("vacation.rdata")
library(car)
#(a)
cat("(a)\n")
alpha <- 0.05
mod1 = lm(miles~income + age + kids,data=vacation)

N <- nrow(vacation)
k <- 4
df <- N-k

b_kids  <- coef(mod1)["kids"]
se_kids <- sqrt(vcov(mod1)["kids","kids"])

tcTwoTail <- qt(1-alpha/2, df)
lower <- b_kids - tcTwoTail * se_kids
upper <- b_kids + tcTwoTail * se_kids
cat("Confidence Interval:[",lower,",",upper,"]\n")

#(b)

plot(vacation$income, resid(mod1),xlab="INCOME",
     ylab="OLS residuals",main="Residuals vs INCOME")
abline(h=0)

plot(vacation$age, resid(mod1),xlab="AGE",
     ylab="OLS residuals",main="Residuals vs AGE")
abline(h=0)

#(c)
cat("(c)\n")
sortedData = vacation[order(vacation$income), ]
first90 <- sortedData[1:90, ]
last90  <- sortedData[(nrow(sortedData)-89):nrow(sortedData), ]

modc1 <- lm(miles~income + age + kids,data=first90)
modc2 <- lm(miles~income + age + kids,data=last90)

df1 <- df.residual(modc1)
df2 <- df.residual(modc2)

sig1sq <- summary(modc1)$sigma^2
sig2sq <- summary(modc2)$sigma^2

fstat <- sig1sq/sig2sq
Flc <- qf(alpha/2, df1, df2)#Left (lower) critical F
Fuc <- qf(1-alpha/2, df1, df2) #Right (upper) critical F
cat("GQ Test Statistic:",fstat,"\n")
cat("Rejection region: R = {F: F<",Flc,"or F>",Fuc,"}\n")

#(d)
cat("(d)\n")
cov1 <- hccm(mod1, type="hc1") #needs package 'car'
robustTest <- coeftest(mod1, vcov.=cov1)

bRob_kids  <- robustTest["kids","Estimate"]
seRob_kids <- robustTest["kids","Std. Error"]

tcRobTwoTail <- qt(1-alpha/2, df)
lowerRob <- bRob_kids - tcRobTwoTail * seRob_kids
upperRob <- bRob_kids + tcRobTwoTail * seRob_kids
cat("Confidence Interval:[",lowerRob,",",upperRob,"]\n")

#(e)
cat("(e)\n")
w <- 1 / (vacation$income^2)
mod2 <- lm(miles~income + age + kids,weights = w,data=vacation)
bCon_kids  <- coef(mod2)["kids"]
seCon_kids <- sqrt(vcov(mod2)["kids","kids"])

tcTwoTail <- qt(1-alpha/2, df)
lowerCon <- bCon_kids - tcTwoTail * seCon_kids
upperCon <- bCon_kids + tcTwoTail * seCon_kids
cat("Conventional GLS: \n")
cat("Confidence Interval:[",lowerCon,",",upperCon,"]\n")

cat("Robust GLS: \n")
cov1 <- hccm(mod2, type="hc1") #needs package 'car'
robustTest <- coeftest(mod2, vcov.=cov1)

bMod2Rob_kids  <- robustTest["kids","Estimate"]
seMod2Rob_kids <- robustTest["kids","Std. Error"]

tcRobTwoTail <- qt(1-alpha/2, df)
lowerMod2Rob <- bMod2Rob_kids - tcRobTwoTail * seMod2Rob_kids
upperMod2Rob <- bMod2Rob_kids + tcRobTwoTail * seMod2Rob_kids
cat("Confidence Interval:[",lowerMod2Rob,",",upperMod2Rob,"]\n")