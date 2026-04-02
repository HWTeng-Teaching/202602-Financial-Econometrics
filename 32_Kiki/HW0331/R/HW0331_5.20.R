# 1. 設定初始參數
set.seed(123)       # 設定亂數種子，確保每次跑出來的結果一樣
N_sizes <- c(100, 500, 1000, 5000)
sigma_sq <- 1000
simulations <- 1000 # 針對每個樣本數，我們模擬 1000 次來估計期望值

# 建立一個空的資料表來儲存最後結果
results <- data.frame(
  N = integer(),
  var_OLS = numeric(),
  var_Mean = numeric(),
  E_sx2_inv = numeric(),
  E_mean_diff_inv = numeric()
)

# 2. 開始針對不同的樣本數 N 進行迴圈
for (N in N_sizes) {
  
  # 建立暫存的向量來存這 1000 次模擬的計算結果
  temp_var_OLS <- numeric(simulations)
  temp_var_Mean <- numeric(simulations)
  temp_sx2_inv <- numeric(simulations)
  temp_mean_diff_inv <- numeric(simulations)
  
  for (i in 1:simulations) {
    # 步驟 a: 從 U(0, 10) 生成隨機變數 x
    x <- runif(N, min = 0, max = 10)
    
    # 步驟 b: 計算 x 的樣本變異數 (注意：題目規定除數使用 N，而不是 R 預設的 N-1)
    s2_x <- sum((x - mean(x))^2) / N
    
    # 計算 (i): OLS 的給定 x 變異數
    temp_var_OLS[i] <- sigma_sq / (N * s2_x)
    
    # 步驟 c: 將 x 排序後切半，計算前後半段的平均
    x_sorted <- sort(x)
    x1_bar <- mean(x_sorted[1:(N/2)])
    x2_bar <- mean(x_sorted[(N/2 + 1):N])
    
    # 計算 (i): Mean estimator 的給定 x 變異數
    temp_var_Mean[i] <- (4 * sigma_sq) / (N * (x2_bar - x1_bar)^2)
    
    # 計算 (ii): 兩個特定算式的單次估計值
    temp_sx2_inv[i] <- 1 / s2_x
    temp_mean_diff_inv[i] <- 4 / (x2_bar - x1_bar)^2
  }
  
  # 3. 將 1000 次的結果取平均 (這就是我們的期望值 E[...] 估計)
  results <- rbind(results, data.frame(
    N = N,
    var_OLS = mean(temp_var_OLS),
    var_Mean = mean(temp_var_Mean),
    E_sx2_inv = mean(temp_sx2_inv),
    E_mean_diff_inv = mean(temp_mean_diff_inv)
  ))
}

# 4. 印出結果
print(results)