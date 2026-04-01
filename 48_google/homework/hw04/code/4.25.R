  # 清除記憶體
  rm(list=ls())

# 載入 POE5Rdata 套件與繪圖套件
if(!require(POE5Rdata)) install.packages("POE5Rdata")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tseries)) install.packages("tseries") # 用於 Jarque-Bera 檢定

library(POE5Rdata)
library(ggplot2)
library(tseries)

# 呼叫數據集
data("collegetown")
df <- collegetown

# --- 1. 模型估計 ---

# (c) 線性模型 (用於比較)
mod_lin <- lm(price ~ sqft, data = df)

# (a) Log-Linear 模型
mod_loglin <- lm(log(price) ~ sqft, data = df)

# (b) Log-Log 模型
mod_loglog <- lm(log(price) ~ log(sqft), data = df)

# --- 2. 數值摘要與預測 ---

# 定義預測點 SQFT = 27 (單位通常為 100 sqft，請根據 .def 檔案確認，此處假設為 27)
x_new <- data.frame(sqft = 27)

# 預測與區間 (需注意 log 模型需轉換回原始尺度)
# 此處先進行估計摘要
print("Log-Linear Model Summary:")
print(summary(mod_loglin))

print("Log-Log Model Summary:")
print(summary(mod_loglog))

# --- 3. 殘差分析與檢定 ---

# Jarque-Bera 檢定
jb_loglin <- jarque.bera.test(residuals(mod_loglin))
jb_loglog <- jarque.bera.test(residuals(mod_loglog))

print("Jarque-Bera Test (Log-Linear):")
print(jb_loglin)

# --- 4. 繪圖輸出 ---

# 圖片 4.25_resid_hist.png (殘差直方圖)
png("4.25_resid_hist.png", width = 800, height = 600)
par(mfrow=c(1,2))
hist(residuals(mod_loglin), main="Log-Linear Residuals", xlab="Residuals")
hist(residuals(mod_loglog), main="Log-Log Residuals", xlab="Residuals")
dev.off()

# 圖片 4.25_resid_plot.png (殘差對 SQFT 散佈圖)
png("4.25_resid_plot.png", width = 800, height = 600)
par(mfrow=c(1,2))
plot(df$sqft, residuals(mod_loglin), main="Log-Linear: Resid vs SQFT")
abline(h=0, col="red")
plot(log(df$sqft), residuals(mod_loglog), main="Log-Log: Resid vs Log(SQFT)")
abline(h=0, col="red")
dev.off()

print(paste("執行完成。圖片已儲存於:", getwd()))