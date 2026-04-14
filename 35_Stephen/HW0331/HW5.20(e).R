#(e)

# 參數設定
N_values <- c(100, 500, 1000, 5000)
sigma2 <- 1000

for (N in N_values) {
  # 針對每個 N，產生「一組」 x ~ U(0,10)
  x <- runif(N, min = 0, max = 10)

  # 排序 x 來計算 x1_bar 和 x2_bar
  x_sorted <- sort(x)
  x1_bar <- mean(x_sorted[1:(N/2)])
  x2_bar <- mean(x_sorted[(N/2 + 1):N])

  # (i) 計算給定這組 x 下的條件變異數
  # R 的 var() 預設除以 N-1，因此這裡手動計算以 N 為分母的變異數
  s_x_sq <- sum((x - mean(x))^2) / N 
  # 另一種寫法是：s_x_sq <- var(x) * (N - 1) / N

  var_b2_ols <- sigma2 / (N * s_x_sq)
  var_b2_mean <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)

  # (ii) 計算期望值的估計值
  s2_inv_est <- 1 / s_x_sq
  mean_diff_est <- 4 / (x2_bar - x1_bar)^2

  # 印出結果
  cat(sprintf("N = %d\n", N))
  cat(sprintf("  (i)  var(b2_ols|x) = %f\n", var_b2_ols))
  cat(sprintf("  (i)  var(b2_mean|x) = %f\n", var_b2_mean))
  cat(sprintf("  (ii) Estimate for E[(s_x^2)^-1] = %f\n", s2_inv_est))
  cat(sprintf("  (ii) Estimate for E[4/(x2_bar - x1_bar)^2] = %f\n\n", mean_diff_est))
}
