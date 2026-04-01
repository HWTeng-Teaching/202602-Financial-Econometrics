  # 清除記憶體
  rm(list=ls())

# 載入必要套件
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# --- 1. 設定模擬參數 ---
set.seed(123)
n_sizes <- c(100, 500, 1000, 5000)
sigma_squared <- 1000

# 建立儲存結果的資料框 (使用小寫蛇型命名)
simulation_results <- data.frame(
  n = n_sizes,
  var_ols = NA,
  var_mean = NA,
  sx2_inv = NA,
  diff_mean_inv = NA
)

# --- 2. 執行模擬計算 ---
for(i in seq_along(n_sizes)) {
  current_n <- n_sizes[i]
  
  # 隨機產生 x ~ Uniform(0, 10)
  x_values <- runif(current_n, 0, 10)
  x_sorted <- sort(x_values)
  
  # 計算分群平均值
  n_half <- current_n / 2
  x1_bar <- mean(x_sorted[1:n_half])
  x2_bar <- mean(x_sorted[(n_half + 1):current_n])
  
  # (i) 計算方差項
  # OLS var(b2|x) = sigma^2 / sum((x - mean(x))^2)
  simulation_results$var_ols[i] <- sigma_squared / sum((x_values - mean(x_values))^2)
  
  # Grouped Mean var(beta_mean|x) = 4*sigma^2 / (n * (x2_bar - x1_bar)^2)
  simulation_results$var_mean[i] <- (4 * sigma_squared) / (current_n * (x2_bar - x1_bar) ^ 2)
  
  # (ii) 計算期望值估計項 (用於 g 小題驗證)
  sample_variance_x <- sum((x_values - mean(x_values))^2) / current_n
  simulation_results$sx2_inv[i] <- 1 / sample_variance_x
  simulation_results$diff_mean_inv[i] <- 4 / (x2_bar - x1_bar)^2
}

# --- 3. 輸出數值結果 (用於 LaTeX 表格) ---
print("--- Simulation Table for Exercise 5.20 ---")
print(simulation_results)

# --- 4. 繪製並儲存趨勢圖 ---
# 轉換為長格式以便 ggplot2 使用
plot_data <- data.frame(
  Sample_Size = rep(n_sizes, 2),
  Variance = c(simulation_results$var_ols, simulation_results$var_mean),
  Estimator = rep(c("OLS (b2)", "Grouped Mean (beta_mean)"), each = length(n_sizes))
)

# 建立圖表
variance_plot <- ggplot(plot_data, aes(x = Sample_Size, y = Variance, color = Estimator, group = Estimator)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_log10() + 
  labs(
    title = "Variance Decay and Consistency Comparison",
    subtitle = paste("Comparison of Estimator Efficiency (sigma^2 =", sigma_squared, ")"),
    x = "Sample Size (N)",
    y = "Variance (Log Scale)"
  ) +
  theme_minimal()

# 顯示於視窗並儲存檔案
print(variance_plot)

# 檔案命名規範：5.20_variance_comparison.png
png("5.20_variance_comparison.png", width = 800, height = 600)
print(variance_plot)
dev.off()

print(paste("執行完成。圖片已儲存於:", getwd()))