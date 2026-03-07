# ==========================================
# 1. 環境清理與數據載入
# ==========================================
rm(list = ls())
graphics.off()
cat("\014")

# 自動載入必要套件
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# 讀取數據 (假設路徑)
data_url <- "http://www.principlesofeconometrics.com/poe5/data/csv/cex5_small.csv"
cex_df <- read.csv(data_url)

# ==========================================
# 2. 敘述統計分析 (Part a & b)
# ==========================================

# (a) 繪製直方圖與計算統計量
hist(cex_df$foodaway, breaks = 40, col = "steelblue", border = "white",
     main = "Distribution of Food Away from Home Expenditure",
     xlab = "Monthly Expenditure per Person ($)")

summary_stats <- summary(cex_df$foodaway)
cat("\n--- (a) FOODAWAY Summary Statistics ---\n")
print(summary_stats)
cat("25th Percentile:", quantile(cex_df$foodaway, 0.25), "\n")
cat("75th Percentile:", quantile(cex_df$foodaway, 0.75), "\n")

# (b) 按教育程度比較 (假設變數名稱為 advanced 與 college)
# 計算各類學歷的外食平均支出
mean_by_edu <- data.frame(
  Advanced = mean(cex_df$foodaway[cex_df$advanced == 1]),
  College = mean(cex_df$foodaway[cex_df$college == 1]),
  Neither = mean(cex_df$foodaway[cex_df$advanced == 0 & cex_df$college == 0])
)
cat("\n--- (b) Mean FOODAWAY by Education ---\n")
print(mean_by_edu)

# ==========================================
# 3. 對數轉換與線性迴歸 (Part c, d, e)
# ==========================================

# (c) 為什麼 ln(FOODAWAY) 的樣本數會變少？
# 解釋：因為 ln(0) 為未定義（負無限大）。若家戶該月支出為 0，則無法取對數。
cex_df_filtered <- subset(cex_df, foodaway > 0)

# (d) 估計模型: ln(FOODAWAY) = b1 + b2*INCOME + e
log_model <- lm(log(foodaway) ~ income, data = cex_df_filtered)
cat("\n--- (d) Regression Results: ln(FOODAWAY) on INCOME ---\n")
print(summary(log_model))

# (e) 繪圖與擬合線
ggplot(cex_df_filtered, aes(x = income, y = log(foodaway))) +
  geom_point(alpha = 0.3, color = "darkgrey") +
  geom_smooth(method = "lm", color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Log Food Expenditure vs Income",
       x = "Household Income ($100 units)",
       y = "ln(FOODAWAY)")

# ==========================================
# 4. 殘差分析 (Part f)
# ==========================================

# 計算並繪製殘差
cex_df_filtered$residuals <- resid(log_model)

ggplot(cex_df_filtered, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuals vs Income",
       subtitle = "Checking for Heteroskedasticity",
       x = "Household Income",
       y = "Least Squares Residuals")