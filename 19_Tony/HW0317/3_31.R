# 3.31

rm(list = ls())
library(POE5Rdata)
# library(ggplot2) 先放著
data(tuna)

# --- (a) Summary Statistics & Time Series Plots ---
# 計算 SAL1 與 APR1 的平均、最小、最大、標準差

summary_stats <- data.frame(
  Variable = c("SAL1", "APR1"),
  Mean = c(mean(tuna$sal1), mean(tuna$apr1)),
  Min = c(min(tuna$sal1), min(tuna$apr1)),
  Max = c(max(tuna$sal1), max(tuna$apr1)),
  SD = c(sd(tuna$sal1), sd(tuna$apr1))
)
print(summary_stats)

# 繪製隨時間變動的圖(時間序列圖)
###取消 僅學習par(mfrow = c(2, 1)) # 分成上下兩格圖###
plot(1:52, tuna$sal1, type = "l", col = "blue", main = "Weekly Sales (SAL1)"
     , xlab="Week", ylab="Sales")
plot(1:52, tuna$apr1, type = "l", col = "red", main = "Weekly Price (APR1)"
     , xlab="Week", ylab="Price")
###取消 僅學習par(mfrow = c(1, 1)) # 恢復單圖模式###

# --- (b) Scatter Plot 散佈圖 ---
plot(tuna$apr1, tuna$sal1, xlab = "Price (APR1)", ylab = "Sales (SAL1)",
     main = "Sales vs Price")

# --- (c) Linear Regression (Unit: Cents) ---
tuna$price1 <- 100 * tuna$apr1  # 換算成 cent
mod_tuna <- lm(sal1 ~ price1, data = tuna)
summary(mod_tuna)



# 提取 Beta2 估計值與 95% 信賴區間
b2_tuna <- coef(mod_tuna)["price1"]
ci_b2 <- confint(mod_tuna, "price1", level = 0.95)

cat("Point Estimate (Beta2): ", b2_tuna, "\n")
cat("95% CI for Beta2: [", ci_b2[1], ",", ci_b2[2], "]\n")

# --- (d) Expected Sales at 70 Cents ---
# 用 predict 函數計算 90% 信賴區間
new_data <- data.frame(price1 = 70)
predict_d <- predict(mod_tuna, new_data, interval = "confidence", level = 0.90)
print(predict_d)

# --- (e) Elasticity at the Means ---
# Elasticity = Beta2 * (mean_p / mean_q)
mean_p <- mean(tuna$price1)
mean_q <- mean(tuna$sal1)
elas_mean <- b2_tuna * (mean_p / mean_q)

# 彈性的標準誤 (假設平均數為常數)
# SE(Elasticity) = SE(Beta2) * (mean_p / mean_q)
se_b2_tuna <- sqrt(vcov(mod_tuna)["price1", "price1"])
se_elas <- se_b2_tuna * (mean_p / mean_q)

# 95% 彈性信賴區間
df_tuna <- df.residual(mod_tuna)
t_crit_95 <- qt(0.975, df_tuna)
lower_elas <- elas_mean - t_crit_95 * se_elas
upper_elas <- elas_mean + t_crit_95 * se_elas

cat("Elasticity at the Means: ", elas_mean, "\n")
cat("95% CI for Elasticity: [", lower_elas, ",", upper_elas, "]\n")

# --- (f) Hypothesis Test: Elasticity = -3 (10% level) ---
# H0: Elasticity = -3, H1: Elasticity != -3
elas_null <- -3
t_stat_f <- (elas_mean - elas_null) / se_elas
p_val_f <- 2 * (1 - pt(abs(t_stat_f), df_tuna))

cat("T-statistic for Hypothesis Test: ", t_stat_f, "\n")
cat("P-value: ", p_val_f, "\n")