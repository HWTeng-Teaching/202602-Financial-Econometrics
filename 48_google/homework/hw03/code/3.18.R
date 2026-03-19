# 清除記憶體 (必須包含)
rm(list=ls())

# 載入 POE5Rdata 套件 (必須包含)
library(POE5Rdata)

# --- 定義題目 3.18 給定參數 ---
n_obs <- 20
df_model <- n_obs - 2  # 自由度 = 18

b1 <- 6.855            # 截距項
b2 <- 3.880            # 斜率項
se_b1 <- 7.383         # 截距項標準誤
se_b2 <- 0.112         # 斜率項標準誤
cov_b1b2 <- -0.746     # 截距與斜率之共變異數估計值

# --- (a) 平均值計算與優化繪圖 ---
avg_income <- 59.3
avg_insurance <- b1 + b2 * avg_income

# 生成回歸線草圖數據
income_seq <- seq(0, 120, length.out = 100)
fit_seq <- b1 + b2 * income_seq

# 💡 修正要點 1：調整邊界 c(bottom, left, top, right)，將右側增加到 10
par(mar = c(5, 5, 4, 10)) 

# 💡 修正要點 2：擴展 xlim 到 160，為右側標籤留出呼吸空間
plot(income_seq, fit_seq, type = "l", col = "red", lwd = 2,
     main = "Simple Linear Regression: Insurance vs Income (Ex 3.18)",
     xlab = "Family Income ($1,000s)", 
     ylab = "Life Insurance Held ($1,000s)",
     xlim = c(0, 160), ylim = c(0, 500), # 擴大 X 軸範圍
     xpd = TRUE) # 允許在繪圖區外繪製

# 3. 標註 Y-Intercept
points(0, b1, col = "darkgreen", pch = 19)
text(0, b1, labels = paste("Y-Intercept =", b1), pos = 4, col = "darkgreen", cex = 0.8)

# 標註 Point of Means
points(avg_income, avg_insurance, col = "blue", pch = 17, cex = 1.2)
text(avg_income, avg_insurance, 
     labels = paste("Point of Means\n(Inc=", avg_income, ")\n(Ins=", round(avg_insurance, 2), ")"), 
     pos = 4, col = "blue", cex = 0.8, offset = 0.5)

# 標註斜率，(X=130, Y=420)
text(130, 420, labels = paste("Slope (b2) =", b2), col = "red", cex = 0.9, font = 2)
grid()

# --- (b) 斜率 beta2 的 95% 信賴區間 ---
t_crit_95 <- qt(0.975, df_model)
ci_b2_95 <- b2 + c(-1, 1) * t_crit_95 * se_b2

# --- (c) Income = 100 的預期值與 99% 信賴區間 ---
x_star <- 100
y_hat_100 <- b1 + b2 * x_star
# Var(y_hat) = Var(b1) + x^2*Var(b2) + 2*x*Cov(b1,b2)
var_y_hat_100 <- (se_b1^2) + (x_star^2 * se_b2^2) + (2 * x_star * cov_b1b2)
se_y_hat_100 <- sqrt(var_y_hat_100)
t_crit_99 <- qt(0.995, df_model)
ci_y100_99 <- y_hat_100 + c(-1, 1) * t_crit_99 * se_y_hat_100

# --- (d) 檢定 H0: beta2 = 5 ---
t_stat_d <- (b2 - 5) / se_b2
p_val_d <- 2 * pt(abs(t_stat_d), df_model, lower.tail = FALSE)

# --- (e) 檢定 H0: beta2 = 1, H1: beta2 > 1 ---
t_stat_e <- (b2 - 1) / se_b2
p_val_e <- pt(t_stat_e, df_model, lower.tail = FALSE)

# ==========================================
# 輸出分析結果摘要 ( Console 輸出內容 )
# ==========================================
cat("\n--- Exercise 3.18 運算結果摘要 ---\n")
cat("(a) 保險金額平均值 (avg_insurance):", avg_insurance, "\n")
cat("(b) 斜率 95% 信賴區間: [", ci_b2_95[1], ",", ci_b2_95[2], "]\n")
cat("(c) X=100 預期值:", y_hat_100, "\n    99% CI: [", ci_y100_99[1], ",", ci_y100_99[2], "]\n")
cat("(d) 檢定 beta2=5: t =", t_stat_d, ", p =", p_val_d, "\n")
cat("(e) 檢定 beta2=1: t =", t_stat_e, ", p =", p_val_e, "\n")