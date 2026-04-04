# 設定參數
set.seed(123) # 固定隨機種子，確保結果可重複
sigma_sq <- 1000
n_values <- c(100, 500, 1000, 5000)

# 建立一個空表格存結果
results <- data.frame(
  N = n_values,
  var_b2_OLS = NA,
  var_beta2_mean = NA,
  estimate_inv_sx2 = NA,
  estimate_mean_diff_part = NA
)

for (i in 1:length(n_values)) {
  n <- n_values[i]
  
  # 1. 產生均勻分佈的 x (0 到 10)
  x <- runif(n, min = 0, max = 10)
  
  # 2. 計算 OLS 的變異數
  var_ols <- sigma_sq / sum((x - mean(x))^2)
  
  # 3. 計算 beta2_mean 的變異數
  x_sorted <- sort(x)
  x1_bar <- mean(x_sorted[1:(n/2)])      # 前一半的平均
  x2_bar <- mean(x_sorted[(n/2 + 1):n])  # 後一半的平均
  var_mean <- (4 * sigma_sq) / (n * (x2_bar - x1_bar)^2)
  
  # 4. 
  sx2 <- sum((x - mean(x))^2) / n
  est_sx2_inv <- 1 / sx2
  est_mean_diff <- 4 / (x2_bar - x1_bar)^2
  
  # 填入表格
  results$var_b2_OLS[i] <- var_ols
  results$var_beta2_mean[i] <- var_mean
  results$estimate_inv_sx2[i] <- est_sx2_inv
  results$estimate_mean_diff_part[i] <- est_mean_diff
}

# 輸出結果
print(results)