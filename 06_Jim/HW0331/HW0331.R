rm(list=ls())
# library(POE5Rdata)
library(stargazer)
library(ggplot2)
library(gridExtra)
# library(modelsummary)
# modelsummary(model_loglin, output = "latex")


set.seed(123) 
sigma2 <- 1000
N_sizes <- c(100, 500, 1000, 5000)

results <- data.frame(
  N = N_sizes,
  var_b2_ols = NA,
  var_b2_mean = NA,
  E_inv_sx2 = NA,
  E_ratio_means = NA
)

for (i in seq_along(N_sizes)) {
  N <- N_sizes[i]
  
  # 1. 產生均勻分佈資料並排序 (為了計算第一半與第二半的平均)
  x <- sort(runif(N, 0, 10))
  
  # 分成兩半
  n <- N / 2
  x1_bar <- mean(x[1:n])
  x2_bar <- mean(x[(n+1):N])
  
  # 2. 計算 (i): var(b2|x) 與 var(beta2_mean|x)
  # OLS var(b2|x) = sigma^2 / SSTx
  sstx <- sum((x - mean(x))^2)
  results$var_b2_ols[i] <- sigma2 / sstx
  
  # Mean Estimator var(beta2_mean|x) = 4*sigma^2 / (N * (x2_bar - x1_bar)^2)
  results$var_b2_mean[i] <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)
  
  # 3. 計算 (ii): 估計期望值項
  # sx^2 使用 N 作為分母 (即樣本二階動差的估計)
  sx2 <- sum((x - mean(x))^2) / N
  results$E_inv_sx2[i] <- 1 / sx2
  results$E_ratio_means[i] <- 4 / (x2_bar - x1_bar)^2
}

# 格式化輸出
print(results)
stargazer(results, summary=FALSE, type="latex", 
          title="Simulation Results", 
          header=FALSE)
