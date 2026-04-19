vacation <- read.csv("~/Desktop/計量/poe5csv/vacation.csv")

# ==========================================
# 準備工作
# ==========================================
# 載入所需套件
library(lmtest)
library(sandwich)

# 假設您的資料已讀取並命名為 vacation (請根據實際情況修改讀取路徑)
# vacation <- read.csv("vacation.csv") 

# ==========================================
# a. OLS 估計與 95% 信賴區間
# ==========================================
cat("\n--- Part a: OLS Model ---\n")
model_ols <- lm(miles ~ income + age + kids, data = vacation)
print(summary(model_ols))

# 建構 KIDS 的 95% 信賴區間
ci_ols <- confint(model_ols, "kids", level = 0.95)
cat("95% CI for KIDS (OLS):\n")
print(ci_ols)

# ==========================================
# b. 繪製殘差圖
# ==========================================
# 取得 OLS 殘差
res_ols <- residuals(model_ols)

# 將畫布置為 1 列 2 欄
par(mfrow = c(1, 2)) 

# 殘差 vs INCOME
plot(vacation$income, res_ols, 
     main = "Residuals vs INCOME", 
     xlab = "INCOME", ylab = "OLS Residuals", pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 殘差 vs AGE
plot(vacation$age, res_ols, 
     main = "Residuals vs AGE", 
     xlab = "AGE", ylab = "OLS Residuals", pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

par(mfrow = c(1, 1)) # 恢復預設畫布

# ==========================================
# c. Goldfeld-Quandt 檢定
# ==========================================
cat("\n--- Part c: Goldfeld-Quandt Test ---\n")
# 根據 INCOME 遞增排序資料
vacation_sorted <- vacation[order(vacation$income), ]

# 前 90 筆 (Low Income) 與最後 90 筆 (High Income) 模型估計
# 樣本總數為 200，所以最後 90 筆是第 111 到 200 筆 (200 - 90 + 1 = 111)
model_low <- lm(miles ~ income + age + kids, data = vacation_sorted[1:90, ])
model_high <- lm(miles ~ income + age + kids, data = vacation_sorted[111:200, ])

# 計算 SSE (殘差平方和)
sse_low <- sum(residuals(model_low)^2)
sse_high <- sum(residuals(model_high)^2)

# F 統計量 (假設變異數隨 INCOME 增加)
gq_f_stat <- sse_high / sse_low

# 自由度 df = n_group - k (90 個樣本 - 4 個參數 = 86)
df_gq <- 90 - 4 

# 計算 p-value
gq_p_value <- 1 - pf(gq_f_stat, df1 = df_gq, df2 = df_gq)

cat("SSE Low (First 90):", sse_low, "\n")
cat("SSE High (Last 90):", sse_high, "\n")
cat("G-Q F-statistic:", gq_f_stat, "\n")
cat("p-value:", gq_p_value, "\n")

# ==========================================
# d. 異質性穩健標準誤 (Heteroskedasticity Robust)
# ==========================================
cat("\n--- Part d: Robust Standard Errors ---\n")
# 取得穩健變異數-共變異數矩陣 (HC1 包含小樣本自由度修正，常被計量教科書採用)
cov_robust <- vcovHC(model_ols, type = "HC1") 
robust_coefs <- coeftest(model_ols, vcov = cov_robust)
print(robust_coefs)

# 手動計算 KIDS 的 95% 穩健信賴區間
beta_kids <- coef(model_ols)["kids"]
se_robust_kids <- sqrt(diag(cov_robust))["kids"]

# 臨界 t 值 (自由度 = 200 - 4 = 196)
df_residual <- model_ols$df.residual
t_critical <- qt(0.975, df = df_residual)

ci_robust_lower <- beta_kids - t_critical * se_robust_kids
ci_robust_upper <- beta_kids + t_critical * se_robust_kids

cat("95% CI for KIDS (Robust OLS): [", ci_robust_lower, ", ", ci_robust_upper, "]\n")

# ==========================================
# e. GLS 估計 (假設變異數正比於 INCOME^2)
# ==========================================
cat("\n--- Part e: GLS (Weighted Least Squares) ---\n")
# 權重設定為變異數的倒數： 1 / (INCOME^2)
model_gls <- lm(miles ~ income + age + kids, weights = 1/(income^2), data = vacation)

# 1. 傳統 GLS 的 95% 信賴區間
ci_gls_conv <- confint(model_gls, "kids", level = 0.95)
cat("95% CI for KIDS (Conventional GLS):\n")
print(ci_gls_conv)

# 2. 穩健 GLS 的 95% 信賴區間
cov_gls_robust <- vcovHC(model_gls, type = "HC1")
beta_gls_kids <- coef(model_gls)["kids"]
se_gls_robust_kids <- sqrt(diag(cov_gls_robust))["kids"]

ci_gls_robust_lower <- beta_gls_kids - t_critical * se_gls_robust_kids
ci_gls_robust_upper <- beta_gls_kids + t_critical * se_gls_robust_kids

cat("95% CI for KIDS (Robust GLS): [", ci_gls_robust_lower, ", ", ci_gls_robust_upper, "]\n")

