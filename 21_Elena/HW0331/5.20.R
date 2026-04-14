# 1. 初始參數設定
sigma2 <- 1000                     
N_values <- c(100, 500, 1000, 5000) # 各種樣本數
nrsim <- 1000                      # Part(ii) 估計期望值時的模擬次數

# 使用 for 迴圈，針對每一個 N 進行計算
for (N in N_values) {
  
  cat("\n======================================================\n")
  cat("當樣本數 N =", N, "時的計算結果：\n")
  cat("======================================================\n")
  
  # -------------------------------------------------------------------
  # (i) 小題：計算給定一組 X 的條件變異數 var(b2|x) 與 var(b2_mean|x)
  # -------------------------------------------------------------------
  set.seed(12345)              # 設定種子碼，確保每次跑出來的 X 都一樣
  x <- runif(N, min=0, max=10) # 從 Uniform(0,10) 產生 N 筆隨機樣本
  
  # a. 計算 LSE 的條件變異數: var(b2|x) = sigma^2 / sum((x - xbar)^2)
  x_bar <- mean(x)
  sum_sq_diff <- sum((x - x_bar)^2)
  var_b2 <- sigma2 / sum_sq_diff
  
  # b. 計算教授的條件變異數: var(b2_mean|x) = 4*sigma^2 / (N * (x2_bar - x1_bar)^2)
  x_sorted <- sort(x)          # 將 x 由小到大排序
  half_N <- N / 2
  x1_bar <- mean(x_sorted[1:half_N])         # 前半段的平均值
  x2_bar <- mean(x_sorted[(half_N + 1):N])   # 後半段的平均值
  
  var_b2_mean <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)
  
  cat("[Part i] 給定單一組 X 的條件變異數：\n")
  cat("  var(b2|x)      =", var_b2, "\n")
  cat("  var(b2_mean|x) =", var_b2_mean, "\n")
  cat("  -> 驗證：LSE 的變異數確實比較小 (高斯-馬可夫定理)\n\n")
  
  # -------------------------------------------------------------------
  # (ii) 小題：透過模擬估計 E[(sx^2)^-1] 與 E[4 / (x2_bar - x1_bar)^2]
  # -------------------------------------------------------------------
  # 建立空的向量來儲存這 1000 次模擬中，每次算出來的數字
  val1 <- numeric(nrsim) 
  val2 <- numeric(nrsim)
  
  # 執行 1000 次的迴圈來逼近期望值
  for (i in 1:nrsim) {
    # 每次都重新產生一組全新的 X
    x_sim <- runif(N, min=0, max=10)
    x_sim_sorted <- sort(x_sim)
    
    # 估計目標 1: (sx^2)^-1 (注意：題目規定 sx^2 是除以 N)
    sx2 <- sum((x_sim - mean(x_sim))^2) / N
    val1[i] <- 1 / sx2
    
    # 估計目標 2: 4 / (x2_bar - x1_bar)^2
    x1_bar_sim <- mean(x_sim_sorted[1:half_N])
    x2_bar_sim <- mean(x_sim_sorted[(half_N + 1):N])
    val2[i] <- 4 / (x2_bar_sim - x1_bar_sim)^2
  }
  
  # 計算這 1000 次的平均值，作為期望值 E[] 的估計
  cat("[Part ii] 進行 1000 次模擬所估計出來的期望值 (無條件變異數的核心)：\n")
  cat("  E[ (s_x^2)^-1 ]              =", mean(val1), "\n")
  cat("  E[ 4 / (x2_bar - x1_bar)^2 ] =", mean(val2), "\n")
  cat("  -> 觀察：當 N 越來越大時，這兩個期望值會收斂在固定的常數附近。\n")
}
