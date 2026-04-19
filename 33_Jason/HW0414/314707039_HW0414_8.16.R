#8.16
#已將vacation data 輸入檔案中
#a小題
model_ols <- lm(miles ~ income + age + kids, data = vacation)
summary(model_ols)
confint(model_ols, "kids", level = 0.95)

#b小題
res_ols <- resid(model_ols)
par(mfrow = c(1, 2))
plot(vacation$income, res_ols, main="Residuals vs Income", xlab="Income", ylab="Residuals")
abline(h=0, col="red")
plot(vacation$age, res_ols, main="Residuals vs Age", xlab="Age", ylab="Residuals")
abline(h=0, col="red")

#c小題
install.packages("lmtest")
library(lmtest)
gq_test <- gqtest(model_ols, order.by = ~ income, fraction = 20, data = vacation)
print(gq_test)

#d小題
install.packages(c("sandwich", "lmtest"))
library(sandwich)
library(lmtest)
coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
b4 <- coef(model_ols)["kids"]
se_robust <- sqrt(diag(vcovHC(model_ols, type = "HC1")))["kids"]
df_rem <- df.residual(model_ols)

ci_lower <- b4 - qt(0.975, df_rem) * se_robust
ci_upper <- b4 + qt(0.975, df_rem) * se_robust
cat("Robust 95% CI for KIDS:", ci_lower, "to", ci_upper, "\n")

#e小題
weights_val <- 1 / (vacation$income^2)
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights_val)
# 常規 GLS 信心區間
ci_gls_regular <- confint(model_gls, "kids", level = 0.95)
cat("常規 GLS (WLS) 的 kids 95% 信賴區間:\n")
print(ci_gls_regular)

# 4. 計算「穩健 GLS」 (Robust WLS) 的 95% 信賴區間
# 實務上為了保險，即便做了 WLS，有時仍會搭配穩健標準誤
# 這裡使用 HC1 修正
se_gls_robust <- sqrt(diag(vcovHC(model_gls, type = "HC1")))["kids"]
b4_gls <- coef(model_gls)["kids"]
df_gls <- df.residual(model_gls)

ci_gls_robust_lower <- b4_gls - qt(0.975, df_gls) * se_gls_robust
ci_gls_robust_upper <- b4_gls + qt(0.975, df_gls) * se_gls_robust

cat("\n穩健 GLS (Robust WLS) 的 kids 95% 信賴區間:\n")
cat(ci_gls_robust_lower, "to", ci_gls_robust_upper, "\n")