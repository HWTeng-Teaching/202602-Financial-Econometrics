library(POE5Rdata)
library(lmtest)
data("vacation")

#a
model <- lm(miles ~ income+age+kids, data=vacation)
summary(model)
confint(model, "kids", level = 0.95)

#b
residuals <- resid(model)
plot(vacation$income, residuals,
     main = "OLS Residuals vs INCOME",
     xlab = "INCOME (in $1000s)",
     ylab = "OLS Residuals",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)

plot(vacation$age, residuals,
     main = "OLS Residuals vs AGE",
     xlab = "AGE",
     ylab = "OLS Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

#c
#income小排到大
vacation_sorted <- vacation[order(vacation$income), ]

#把中間20筆拿掉，保留first90和last90
group1 <- vacation_sorted[1:90, ]
group2 <- vacation_sorted[111:200, ]  # 共200筆，所以後90是從111開始

#分別估計OLS模型
model1 <- lm(miles ~ income+age+kids, data=group1)
model2 <- lm(miles ~ income+age+kids, data=group2)

#RSS
RSS1 <- sum(resid(model1)^2)
RSS2 <- sum(resid(model2)^2)

#F統計量
F_stat <- RSS2 / RSS1

#比對臨界值或計算p-value
dflast <- model2$df.residual  #後組df
dffirst <- model1$df.residual  #前組df
p_value <- 1 - pf(F_stat, dflast, dffirst)

cat("F-statistic =", F_stat, "\np-value =", p_value, "\n")

#d
model <- lm(miles ~ income+age+kids, data=vacation)

#robust標準誤差
robust_se <- vcovHC(model, type = "HC1")
coeftest(model, vcov=robust_se)

#KIDS的95%信賴區間
confint_robust <- coefci(model, vcov. = robust_se, level=0.95)
confint_robust["kids", ]

#e
#權重：w_i=1 / income_i
weights <- 1 / vacation$income

#使用WLS估計模型，等同於GLS
gls_model <- lm(miles ~ income + age + kids, data = vacation, weights = weights)
summary(gls_model)
confint_conventional <- confint(gls_model)["kids", ]

robust_se_gls <- vcovHC(gls_model, type = "HC1")
confint_robust <- coefci(gls_model, vcov. = robust_se_gls, level = 0.95)["kids", ]

cat("Conventional GLS C.I.（kids）: \n")
print(confint_conventional)
cat("\nRobust GLS C.I.（kids）: \n")
print(confint_robust)