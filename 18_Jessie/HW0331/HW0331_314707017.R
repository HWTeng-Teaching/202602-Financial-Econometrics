# 設定參數
sigma2 <- 1000
sample_sizes <- c(100, 500, 1000, 5000)
results <- data.frame()

set.seed(123) # 設定隨機種子以利重複實驗

for (N in sample_sizes) {
  # 1. 產生 x 資料：從 U(0, 10) 抽樣並排序
  x <- sort(runif(N, min = 0, max = 10))
  
  # 2. 計算 OLS 的條件變異數: var(b2|x) = sigma^2 / sum((x_i - mean(x))^2)
  var_ols <- sigma2 / sum((x - mean(x))^2)
  
  # 3. 分成兩半計算平均值
  x1_bar <- mean(x[1:(N/2)])
  x2_bar <- mean(x[(N/2 + 1):N])
  
  # 4. 計算 Mean 估計式的條件變異數: var(beta_mean|x) = 4 * sigma^2 / (N * (x2_bar - x1_bar)^2)
  var_mean <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)
  
  # 5. 計算 (ii) 小題要求的期望值估算項
  # s_x^2 使用 N 作為分母 (如題目所述)
  sx2 <- sum((x - mean(x))^2) / N
  term_ii_1 <- 1 / sx2
  term_ii_2 <- 4 / (x2_bar - x1_bar)^2
  
  # 儲存結果
  results <- rbind(results, data.frame(
    N = N,
    Var_OLS = var_ols,
    Var_Mean = var_mean,
    Inv_Sx2 = term_ii_1,
    Four_Diff2 = term_ii_2
  ))
}

# 格式化輸出結果
print("模擬結果比較表：")
print(results)

# 額外計算：相對效率 (OLS 永遠比較有效)
results$Efficiency_Ratio <- results$Var_OLS / results$Var_Mean
print("OLS 與 Mean 估計式的效率比：")
print(results[, c("N", "Efficiency_Ratio")])