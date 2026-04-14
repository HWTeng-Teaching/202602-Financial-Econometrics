#5.20
# 母體sigma已知
sigma2 <- 1000
N_list <- c(100, 500, 1000, 5000)

# 準備一個容器來存結果
results <- data.frame()

set.seed(123) #隨機
for (N in N_list) {
  # 1. 生成 x：從 Uniform(0, 10) 抽出 N 個點
  x <- runif(N, min = 0, max = 10)
  
  # 2. 排序 x (題目要求：ordered according to increasing values)
  x_sorted <- sort(x)
  
  # 3. 計算 OLS 的 var(b2|x) = sigma^2 / SSTx
  SSTx <- sum((x_sorted - mean(x_sorted))^2)
  var_ols <- sigma2 / SSTx
  
  # 4. 計算分組平均法的 var(beta2_mean|x)
  # 分成兩半
  x1_bar <- mean(x_sorted[1:(N/2)])
  x2_bar <- mean(x_sorted[(N/2 + 1):N])
  var_mean <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)
  cat(var_ols, var_mean)
  # 5. 計算 (e)(ii) 的兩個估計值
  sx2 <- sum((x_sorted - mean(x_sorted))^2) / N # 用 N 當除數
  inv_sx2 <- 1 / sx2 #倒數
  part_ii_val <- 4 / (x2_bar - x1_bar)^2
  
  # 存入結果
  results <- rbind(results, data.frame(
    N = N,
    Var_OLS = var_ols,
    Var_Mean = var_mean,
    Inv_sx2 = inv_sx2,
    Ratio_Part_ii = part_ii_val
  ))
}

# 顯示結果
print(results)
