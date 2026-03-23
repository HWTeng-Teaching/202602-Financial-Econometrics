setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("collegetown.rdata")

# a
# 提取參數與設定信心水準
model1 <- lm(price ~ I(sqft^2), data = collegetown)
alpha2_hat <- coef(model1)["I(sqft^2)"]
se_alpha2  <- summary(model1)$coefficients["I(sqft^2)", "Std. Error"]
df  <- df.residual(model1)
alpha_level <- 0.05

# 計算檢定統計量 (t0)
t0 <- (alpha2_hat - 13/40) / se_alpha2

# Reject Region
tc <- qt(1 - alpha_level, df = df)

# P-value
p_val <- pt(t0, df = df, lower.tail = FALSE)

cat("Hypotheses\n")
cat("   H0: 40 * alpha2 <= 13\n")
cat("   H1: 40 * alpha2 > 13\n\n")

cat("Test Statistic (t0)\n")
cat("   t0 =", t0, "\n\n")

cat("Reject Region\n")
cat("   Reject H0 if t >", tc, "\n\n")

cat("P-value\n")
cat("   p-value =", p_val, "\n\n")

cat("Conclusion\n")
if (p_val < alpha_level) {
  cat("   Conclusion: p-value < 0.05, Reject H0.\n")
} else {
  cat("   Conclusion: p-value > 0.05, Don't reject H0.\n")
}


#b
# 計算檢定統計量 (t_b)
t_b <- (alpha2_hat - 13/80) / se_alpha2

# P-value 計算 (右尾)
p_val_b <- pt(t_b, df = df, lower.tail = FALSE)

cat("\n--- Part (b): Marginal Effect at 4,000 sqft ---\n")
cat("Hypotheses\n")
cat("   H0: 80 * alpha2 <= 13\n   H1: 80 * alpha2 > 13\n")
cat("Test Statistic (t0) =", t_b, "\n")
cat("Reject Region: t >", tc, "\n")
cat("p-value =", p_val_b, "\n")
cat("Conclusion\n")
if (p_val_b < alpha_level) {
  cat("   Conclusion: p-value < 0.05, Reject H0.\n")
} else {
  cat("   Conclusion: p-value > 0.05, Don't reject H0.\n")
}


# --- 3.23 (c) 預測 2,000 sqft 的價格與 95% 信賴區間 ---
# 使用 predict 函數獲取結果
pred_c <- predict(model1, newdata = data.frame(sqft = 20), interval = "confidence", level = 0.95)

cat("\n--- Part (c): Prediction for 2,000 sqft ---\n")
cat("Point Estimate (Expected Price)\n")
cat("   E(Price | SQFT=20) =", pred_c[1, "fit"], " ($", pred_c[1, "fit"]*1000, ")\n")
cat("95% Confidence Interval\n")
cat("   Interval: [", pred_c[1, "lwr"], ",", pred_c[1, "upr"], "]\n")
cat("   Conclusion: We are 95% confident that the true average price is in this range.\n")


# --- 3.23 (d) 驗證樣本觀測值與區間的相容性 ---

# 1. 計算樣本中 SQFT=20 的平均價格
sample_mean_20 <- mean(subset(collegetown, sqft == 20)$price)

# 2. 判斷是否落在區間內
is_in_range <- sample_mean_20 >= pred_c[1, "lwr"] && sample_mean_20 <= pred_c[1, "upr"]

cat("\n--- Part (d): Compatibility Check ---\n")
cat("Calculate Sample Mean for SQFT = 20\n")
cat("   Sample Mean =", sample_mean_20, "\n")
cat("Check if Sample Mean is within 95% CI from Part (c)\n")
if (is_in_range) {
  cat("   Result: ", sample_mean_20, " is within [", pred_c[1, "lwr"], ",", pred_c[1, "upr"], "]\n")
  cat("   Conclusion: The result is COMPATIBLE with the prediction in (c).\n")
} else {
  cat("   Result: ", sample_mean_20, " is NOT within the interval.\n")
  cat("   Conclusion: The result is INCOMPATIBLE with the prediction in (c).\n")
}