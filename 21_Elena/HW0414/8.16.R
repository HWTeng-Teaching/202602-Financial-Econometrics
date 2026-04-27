setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("vacation.rdata")

# (a)
# 估計 OLS 模型
vac_ols <- lm(miles ~ income + age + kids, data = vacation)
summary(vac_ols)

# 95% 信賴區間 

ci_95 <- confint(vac_ols, level = 0.95)
print(ci_95)


# (b)
# 萃取出 OLS 模型的殘差
vac_res <- resid(vac_ols)

# 「殘差 vs 所得 (income)」散佈圖 
plot(vac_ols$model$income, vac_res, 
     main = "Residuals vs Income", 
     xlab = "Income ($1000 units)", 
     ylab = "OLS Residuals",
     pch = 16, col = "steelblue")
abline(h = 0, col = "red", lwd = 2) 

# 「殘差 vs 年齡 (age)」散佈圖
plot(vac_ols$model$age, vac_res, 
     main = "Residuals vs Age", 
     xlab = "Average Age", 
     ylab = "OLS Residuals",
     pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)


# (c)
#  income 由小到大對資料進行排序
vacation_sorted <- vacation[order(vacation$income), ]

# 切割資料：前 90 筆 (低所得) 與 後 90 筆 (高所得)
data_low <- vacation_sorted[1:90, ]
data_high <- vacation_sorted[111:200, ]

# 對兩組資料跑 OLS 迴歸
mod_low <- lm(miles ~ income + age + kids, data = data_low)
mod_high <- lm(miles ~ income + age + kids, data = data_high)

# 取得兩組的殘差變異數 (sigma^2) 與自由度
df_low <- mod_low$df.residual    # 應為 86
df_high <- mod_high$df.residual  # 應為 86

sigma2_low <- (summary(mod_low)$sigma)^2
sigma2_high <- (summary(mod_high)$sigma)^2

# 計算 GQ 檢定的 F 統計量 (大變異數放分子)
f_stat <- sigma2_high / sigma2_low

# 尋找 F 分配的右尾臨界值 (alpha = 0.05)
alpha <- 0.05
f_critical <- qf(1 - alpha, df_high, df_low)
p_value <- 1 - pf(f_stat, df_high, df_low)

# 輸出最終結果
cat("Low Income Variance:", sigma2_low, "\n")
cat("High Income Variance:", sigma2_high, "\n")
cat("GQ F-statistic:", f_stat, "\n")
cat("Critical Value (5%):", f_critical, "\n")
cat("P-value:", p_value, "\n")

# 判斷結論
if(f_stat > f_critical) {
  cat("Conclusion: Reject H0. There is evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion: Do not reject H0. Not enough evidence to show heteroskedasticity.\n")
}


# (d)
install.packages("lmtest")
library(car)
library(lmtest)

# 確認已跑過原本的 OLS 模型
vac_ols <- lm(miles ~ income + age + kids, data = vacation)

# 計算 White 強健性共變異數矩陣 (設定 type = "hc1")
cov_hc1 <- hccm(vac_ols, type="hc1")

# 輸出包含「強健性標準誤」的迴歸報表
robust_results <- coeftest(vac_ols, vcov. = cov_hc1)
print(robust_results)

# 建構 95% 的強健性信賴區間 (Robust Confidence Interval)
ci_robust <- coefci(vac_ols, vcov. = cov_hc1, level = 0.95)
print(ci_robust)


# (e)
# 定義 GLS 的權重 (變異數為 income 的平方，權重為其倒數)
w <- 1 / (vacation$income^2)

# 估計 GLS (WLS) 模型
vac_gls <- lm(miles ~ income + age + kids, weights = w, data = vacation)

# 取得「傳統 GLS」的 95% 信賴區間
ci_gls_regular <- confint(vac_gls, level = 0.95)
cat("--- Conventional GLS 95% CI for kids ---\n")
print(ci_gls_regular["kids", ])

# 計算並取得「強健性 GLS (Robust GLS)」的 95% 信賴區間
cov_gls_hc1 <- hccm(vac_gls, type = "hc1")
ci_gls_robust <- coefci(vac_gls, vcov. = cov_gls_hc1, level = 0.95)
cat("\n--- Robust GLS 95% CI for kids ---\n")
print(ci_gls_robust["kids", ])
