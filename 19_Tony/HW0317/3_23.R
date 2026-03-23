# 3.23

rm(list = ls())
library(POE5Rdata)
data(collegetown)
?collegetown


## a

# 1. 建立二次模型
# PRICE = α1 + α2SQFT^2 + e

mod_quadratic <- lm(price ~ I(sqft^2), data = collegetown) 
# I(sqft^2)做一般的數學運算，避免做成交互作用
result <- summary(mod_quadratic)
result


# 3. 提取 alpha2 估計值與標準誤
# coef() 抓係數, vcov() 抓變異數矩陣
a2_hat <- coef(mod_quadratic)["I(sqft^2)"]
se_a2  <- sqrt(vcov(mod_quadratic)["I(sqft^2)", "I(sqft^2)"])

# 4. 計算 2000 sqft (sqft=20) 時的邊際效應 (ME = 2 * a2 * 20 = 40 * a2)
me_observed <- 40 * a2_hat
claim_value <- 13  

# 5. 計算t檢定值 (基於邊際效應)
t_stat <- (me_observed - claim_value) / (40 * se_a2)

# 6. 提取自由度與計算 p-value
df_quadratic <- df.residual(mod_quadratic)
p_value <- 1 - pt(t_stat, df_quadratic)
t_critical <- qt(0.95, df_quadratic)

# 7. 輸出結果
cat("Marginal Effect at 2000 sqft: ", round(me_observed, 4), "\n")
cat("Test Statistic (t): ", round(t_stat, 4), "\n")
cat("P-value: ", round(p_value, 6), "\n")
cat("Critical Value (tc): ", round(t_critical, 4), "\n")

# 8. 結論判斷
if (t_stat > t_critical) {
  cat("Conclusion: Reject H0. The marginal effect is significantly greater than $13,000.\n")
} else {
  cat("Conclusion: Do not reject H0. No evidence that marginal effect exceeds $13,000.\n")
}


## b

# 1. 計算 Marginal Effect at 4000 sqft (SQFT = 40)
# (ME = 2 * a2 * 40 = 80 * a2)
# 使用 me_40 變數名，區隔不同坪數
me_40 <- 80 * a2_hat
claim_value <- 13  

# 2. 計算t檢定值 (Test Statistic)
t_stat_b <- (me_40 - claim_value) / (80 * se_a2) # 給自己的提醒：標準誤須乘上倍數 80

# 3. 提取自由度與計算 p-value (單尾右側)
df_quadratic <- df.residual(mod_quadratic)
p_value_b <- 1 - pt(t_stat_b, df_quadratic)
t_critical_b <- qt(0.95, df_quadratic) # 單尾 5% 臨界值

# 4. 顯示結果
cat("Marginal Effect at 4000 sqft: ", round(me_40, 4), "\n")
cat("Test Statistic (t): ", round(t_stat_b, 4), "\n")
cat("P-value: ", round(p_value_b, 6), "\n")
cat("Critical Value (tc): ", round(t_critical_b, 4), "\n")

# 5. 結論判斷
if (t_stat_b > t_critical_b) {
  cat("Conclusion: Reject H0. The marginal effect is significantly greater than $13,000.\n")
} else {
  cat("Conclusion: Do not reject H0. No evidence that marginal effect exceeds $13,000.\n")
}


## c
# Expected Price and 95% Interval for 2000 sqft (SQFT = 20)

# 1. 提取係數與共變異數矩陣
a1_hat <- coef(mod_quadratic)["(Intercept)"]
a2_hat <- coef(mod_quadratic)["I(sqft^2)"]
v_cov_matrix <- vcov(mod_quadratic)

# 2. 點估計值 (Point Estimate)
# 當 SQFT = 20, SQFT^2 = 400
sqft_value <- 20
sqft_sq <- sqft_value^2  # 400
price_expected <- a1_hat + a2_hat * sqft_sq

# 3. 計算期望值的標準誤
# Var(a1 + 400*a2) = Var(a1) + 2 * 400 * Cov(a1, a2) + 400^2 * Var(a2) 
var_price <- v_cov_matrix["(Intercept)", "(Intercept)"] + 
  2 * sqft_sq * v_cov_matrix["(Intercept)", "I(sqft^2)"] +
  (sqft_sq^2) * v_cov_matrix["I(sqft^2)", "I(sqft^2)"]

se_price <- sqrt(var_price)

# 4. 找出 95% t 臨界值 (雙尾)
df_quadratic <- df.residual(mod_quadratic)
t_critical_c <- qt(0.975, df_quadratic)

# 5. 計算信賴區間
lower_c <- price_expected - t_critical_c * se_price
upper_c <- price_expected + t_critical_c * se_price

# 6. 輸出結果
cat("Expected Price for 2000 sqft: $", round(price_expected, 4), " (in $1000)\n", sep="")
cat("95% Confidence Interval: [", round(lower_c, 4), ", ", round(upper_c, 4), "]\n", sep="")


## d

# 1. 篩選所有 SQFT = 20 的房屋資料
houses_20 <- subset(collegetown, sqft == 20)
n_houses_20 <- nrow(houses_20) # 列出房屋數量

# 2. 計算樣本平均售價
sample_mean_20 <- mean(houses_20$price)


# 3. 輸出結果
cat("Number of houses with SQFT = 20: ", n_houses_20, "\n")
cat("Sample average price for SQFT = 20: $", round(sample_mean_20, 4), " (in $1000)\n", sep="")
cat("Predicted price from part (c): $", round(price_expected, 4), " (in $1000)\n", sep="")

# 4. 檢查相容性 (是否落在區間內)
is_compatible <- sample_mean_20 >= lower_c && sample_mean_20 <= upper_c

if (is_compatible) {
  cat("Conclusion: The sample average is COMPATIBLE with the result in part (c).\n")
} else {
  cat("Conclusion: The sample average is NOT compatible with the result in part (c).\n")
}