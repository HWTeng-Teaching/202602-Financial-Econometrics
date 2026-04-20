library(POE5Rdata)
library(sandwich)
library(lmtest)
library(dplyr)

data("vacation")

# --- (a) OLS and 95% CI ---
ols_reg <- lm(miles ~ income + age + kids, data = vacation)
ci_a <- confint(ols_reg, "kids", level = 0.95)

# --- (b) Residual Plot ---
# 觀察殘差是否隨所得或年齡增加而擴散
res <- resid(ols_reg)
par(mfrow=c(1,2))
plot(vacation$income, res, main="Residuals vs Income", xlab="Income", ylab="OLS Residuals")
abline(h=0, col="red", lty = 2)
plot(vacation$age, res, main="Residuals vs Age", xlab="Age", ylab="OLS Residuals")
abline(h=0, col="red", lty = 2)

# --- (c) GQ Test ---
# 步驟：依所得排序，取前 90 與後 90 筆觀測值（中間剔除 20 筆）
vac_sorted <- vacation %>% arrange(income)
low_90 <- vac_sorted[1:90, ]
high_90 <- vac_sorted[111:200, ]

ols_low <- lm(miles ~ income + age + kids, data = low_90)
ols_high <- lm(miles ~ income + age + kids, data = high_90)

# F 統計量 (df = 90 - 4 = 86)
f_stat <- (deviance(ols_high)/86) / (deviance(ols_low)/86)
p_val_gq <- pf(f_stat, 86, 86, lower.tail = FALSE)

# --- (d) Robust SE ---
# 使用 HC1 (Stata 預設類型) 進行修正
robust_cov <- vcovHC(ols_reg, type = "HC1")
robust_se <- coeftest(ols_reg, vcov = robust_cov)

#  95% CI: b_k +/- 1.97 * rob_se
t_crit <- qt(0.975, 196)
kids_coef <- coef(ols_reg)["kids"]
kids_rob_se <- robust_se["kids", "Std. Error"]
ci_d <- c(kids_coef - t_crit * kids_rob_se, kids_coef + t_crit * kids_rob_se)

# --- (e) GLS/WLS 估計 ---
# 假設變異數與所得的平方成正比，權重為 1/income^2
gls_reg <- lm(miles ~ income + age + kids, data = vacation, weights = 1/income^2)

# 傳統 GLS 標準誤 CI
ci_e_conv <- confint(gls_reg, "kids", level = 0.95)

# 穩健 GLS 標準誤 CI
gls_robust_cov <- vcovHC(gls_reg, type = "HC1")
gls_rob_se <- coeftest(gls_reg, vcov = gls_robust_cov)
kids_gls_coef <- coef(gls_reg)["kids"]
kids_gls_rob_se <- gls_rob_se["kids", "Std. Error"]
ci_e_robust <- c(kids_gls_coef - t_crit * kids_gls_rob_se, kids_gls_coef + t_crit * kids_gls_rob_se)

# 結果
cat("--- 95% Confidence Intervals for KIDS ---\n")
cat("(a) OLS (Conventional):", ci_a, "\n")
cat("(d) OLS (Robust):      ", ci_d, "\n")
cat("(e) GLS (Conventional):", ci_e_conv, "\n")
cat("(e) GLS (Robust):      ", ci_e_robust, "\n")