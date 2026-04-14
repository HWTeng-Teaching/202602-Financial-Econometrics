# 設定題目給定的常數
N_values <- c(100, 500, 1000, 5000)
sigma2 <- 1000

# 建立一個空的資料框 (Data Frame) 來儲存每次迴圈的結果
results <- data.frame(
  N = integer(),
  var_b2 = numeric(),         # OLS 估計式的變異數
  var_b2_mean = numeric(),    # b2_mean 估計式的變異數
  est_E_s2_inv = numeric(),   # (s_x^2)^(-1) 的估計值 (理論值趨近 0.12)
  est_E_diff_inv = numeric()  # 4/(x2_bar - x1_bar)^2 的估計值 (理論值趨近 0.16)
)

# 設定隨機種子 (Random Seed) 以確保每次執行的抽樣結果一致
set.seed(123)

# 開始針對不同的樣本數進行運算
for (N in N_values) {
  
  # 1. 從均勻分配 U(0, 10) 中隨機抽出 N 個觀察值
  x <- runif(N, min = 0, max = 10)
  
  # 2. 將 x 依照數值大小由小到大排序
  x_sorted <- sort(x)
  
  # 3. 將排序後的 x 分為前半段 (x1) 與後半段 (x2)
  half_N <- N / 2
  x1 <- x_sorted[1:half_N]
  x2 <- x_sorted[(half_N + 1):N]
  
  # 計算整體的平均數以及兩半部的平均數
  x_bar <- mean(x)
  x1_bar <- mean(x1)
  x2_bar <- mean(x2)
  
  # 4. 計算樣本變異數 s_x^2 (注意：題目指示除數使用 N 而非 N-1)
  s2_x <- sum((x - x_bar)^2) / N
  
  # ------------------ (i) 計算估計式的變異數 ------------------
  # OLS 變異數公式
  var_b2 <- sigma2 / (N * s2_x)  
  
  # 替代估計式 b2_mean 的變異數公式
  var_b2_mean <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)
  
  # ------------------ (ii) 計算期望值的估計項 ------------------
  est_E_s2_inv <- 1 / s2_x
  est_E_diff_inv <- 4 / (x2_bar - x1_bar)^2
  
  # 將該次 N 的結果存入資料框中
  results <- rbind(results, data.frame(
    N = N,
    var_b2 = round(var_b2, 4),
    var_b2_mean = round(var_b2_mean, 4),
    est_E_s2_inv = round(est_E_s2_inv, 4),
    est_E_diff_inv = round(est_E_diff_inv, 4)
  ))
}

# 輸出最終結果表格
print(results)
