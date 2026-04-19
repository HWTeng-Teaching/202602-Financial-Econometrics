# ==========================================
# 8.16 Vacation Model 
# ==========================================

# 1. 
if (!require("sandwich")) install.packages("sandwich")
library(sandwich)

# 2. 
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
load(url(url)) 
df <- vacation

# ------------------------------------------
# (a) OLS 估計
# ------------------------------------------
model_ols <- lm(miles ~ income + age + kids, data = df)
cat("--- (a) OLS 95% CI (Kids) ---\n")
print(confint(model_ols, "kids", level = 0.95))

# ------------------------------------------
# (c) Goldfeld-Quandt 檢定
# ------------------------------------------
# 步驟 1: 依所得排序
df_sorted <- df[order(df$income), ]

# 步驟 2: 分成前 90 筆 (Low Income) 與 後 90 筆 (High Income)
low_sub <- df_sorted[1:90, ]
high_sub <- df_sorted[111:200, ] # 中間剔除 20 筆 (200 - 90 - 90 = 20)

# 步驟 3: 分別跑回歸
fit_low <- lm(miles ~ income + age + kids, data = low_sub)
fit_high <- lm(miles ~ income + age + kids, data = high_sub)

# 步驟 4: 計算 F 統計量 (兩組變異數之比，用 SSR / df)
# 自由度 df = n - k = 90 - 4 = 86
ssr_low <- sum(resid(fit_low)^2)
ssr_high <- sum(resid(fit_high)^2)
f_stat <- (ssr_high / 86) / (ssr_low / 86) # 簡化成 ssr_high / ssr_low

# 步驟 5:  5% 臨界值
f_crit <- qf(0.95, 86, 86)

cat("\n--- (c) Goldfeld-Quandt Test  ---\n")
cat(sprintf("F-statistic: %f\n", f_stat))
cat(sprintf("5%% Critical Value: %f\n", f_crit))
cat(if(f_stat > f_crit) "結果：拒絕 H0，存在異質變異數。\n" else "結果：無法拒絕 H0。\n")

# ------------------------------------------
# (d) Robust 95% CI 
# ------------------------------------------
robust_cov <- vcovHC(model_ols, type = "HC1")
se_kids_robust <- sqrt(robust_cov["kids", "kids"])
tcrit <- qt(0.975, df.residual(model_ols))
beta_kids <- coef(model_ols)["kids"]

lower_d <- beta_kids - tcrit * se_kids_robust
upper_d <- beta_kids + tcrit * se_kids_robust

cat("\n--- (d) Robust 95% CI ---\n")
cat(sprintf("[%f, %f]\n", lower_d, upper_d))

# ------------------------------------------
# (e) GLS (WLS) 與 Robust GLS
# ------------------------------------------
model_gls <- lm(miles ~ income + age + kids, data = df, weights = 1/income^2)

# GLS Robust CI
robust_gls_cov <- vcovHC(model_gls, type = "HC1")
se_kids_gls_robust <- sqrt(robust_gls_cov["kids", "kids"])
beta_kids_gls <- coef(model_gls)["kids"]

lower_e_robust <- beta_kids_gls - tcrit * se_kids_gls_robust
upper_e_robust <- beta_kids_gls + tcrit * se_kids_gls_robust

cat("\n--- (e) GLS 結果 ---\n")
cat("傳統 GLS CI:\n"); print(confint(model_gls, "kids", level = 0.95))
cat(sprintf("Robust GLS CI:\n [%f, %f]\n", lower_e_robust, upper_e_robust))
