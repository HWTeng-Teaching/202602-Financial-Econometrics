# 3.18

rm(list = ls())
library(POE5Rdata)


## a
b1 <- 6.855
b2 <- 3.880
income_mean <- 59.3
insurance_mean  <- b1 + b2 * income_mean  #計算平均保額
# y_hat = 6.855 + 3.880 * x_hat
insurance_mean

# 繪圖
# 1. 畫擬合線
curve(b1 + b2 * x, from = -10, to = 80, #設定範圍方便顯示
     xlab = "Income ($1000)", 
     ylab = "Insurance Held ($1000)", 
     main = "Fitted Relationship: Insurance vs Income",
     col = "red", lwd = 2)

# 2. 加入座標軸與輔助虛線，標示出平均點的座標
abline(v = 0, h = 0, lty = 1)
abline(v = income_mean, h = insurance_mean, lty = 2, col = "gray")

# 3. 標註截距點、平均點、截距與斜率
points(0, b1, col = "blue", pch = 20, cex = 1)
text(0, b1, labels = " y-intercept (0, 6.855)", pos = 4, col = "blue")
points(income_mean, insurance_mean, col = "blue", pch = 18, cex = 1)
text(income_mean, insurance_mean, labels = " Mean Point (59.3, 236.939)", pos = 2, col = "blue")
text(72, 50, labels = paste("Slope =", b2), col = "red")
text(72, 20, labels = paste("Intercept =", b1), col = "red")

## b
# 1. Point Estimate

point_estimate <- b2
point_estimate

# 2. 95% Interval Estimate
se_b2 <- 0.112 # 斜率標準誤
n <- 20
df <- n - 2
t_critical <- qt(0.975, df) 

lower_b2 <- b2 - t_critical * se_b2
upper_b2 <- b2 + t_critical * se_b2

# 3. 輸出結果
cat("\n=== Question 3.18 (b) ===", "\n")
cat("Point Estimate: ", point_estimate, "\n")
cat("95% Interval Estimate: [", round(lower_b2, 4), ", ", round(upper_b2, 4), "]\n\n")

## c
# 1. 定義已知數值
x0 <- 100 # 收入 100,000 / 1,000
se_b1 <- 7.383 # 截距標準誤
cov_b1b2 <- -0.746 # 截距與斜率的共變異數

# 2. Point Estimate
y0_hat <- b1 + b2 * x0

# 3. 計算預測值的變異數與標準誤
## Var(y_hat) = Var(b1) + 2 * x0 * Cov(b1, b2) + x0^2 * Var(b2) 
var_y0 <- (se_b1^2) + (2 * x0 * cov_b1b2) + (x0^2 * se_b2^2) 
se_y0 <- sqrt(var_y0)

# 4. 找出 99% t 臨界值 (df = 18, 雙尾各 0.005)
t_critical_99 <- qt(0.995, df)

# 5. 計算 99% 信賴區間
lower_c <- y0_hat - t_critical_99 * se_y0
upper_c <- y0_hat + t_critical_99 * se_y0

# 6. 輸出結果
cat("\n=== Question 3.18 (c) ===", "\n")
cat("Point Estimate at Income=100: ", y0_hat, "\n")
cat("99% Interval Estimate: [", round(lower_c, 4), ",", round(upper_c, 4), "]\n\n")

## d
# 1. 定義t檢定參數
b2_claim <- 5 # 董事會成員主張的值
se_b2 <- 0.112 # 斜率標準誤 (來自題目)
alpha <- 0.05 # 顯著水準
n <- 20
df <- n - 2

# 2. 計算t檢定值
t_stat <- (b2 - b2_claim) / se_b2

# 3. 找出臨界值與拒絕域
t_critical_d <- qt(1 - alpha/2, df)

# 4. 顯示結果
cat("\n=== Question 3.18 (d) ===", "\n")
cat("Test Statistic (t): ", t_stat, "\n")
cat("Critical Value (tc): ", t_critical_d, "\n")

# 5. 判斷是否拒絕 H0
if (abs(t_stat) > t_critical_d) {
  cat("Result: Reject H0. The data do NOT support the claim.\n")
} else {
  cat("Result: Do not reject H0. The data support the claim.\n")
}

## e
# 1. 定義單尾檢定參數
b2_null <- 1      # 虛無假設的主張值
alpha_e <- 0.01   # 1% 顯著水準

# 2. 計算t檢定值
t_stat_e <- (b2 - b2_null) / se_b2
# (估計值 3.88 - 主張值 1) / 標準誤 0.112

# 3. 找出單尾檢定臨界值
t_critical_e <- qt(1 - alpha_e, df) 
# qt(0.99, df) 代表右邊留下 1% 的面積

# 4. 顯示結果
cat("\n=== Question 3.18 (e) ===", "\n")
cat("Test Statistic (t): ", t_stat_e, "\n")
cat("Critical Value (tc) for 1% one-tail: ", t_critical_e, "\n")

# 5. 判斷是否拒絕 H0
if (t_stat_e > t_critical_e) {
  cat("Result: Reject H0. The slope is significantly larger than 1.\n")
} else {
  cat("Result: Do not reject H0. The slope is not significantly larger than 1.\n")
}
