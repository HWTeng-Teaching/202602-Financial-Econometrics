library(lmtest)
library(sandwich)

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
temp_file <- tempfile(fileext = ".rdata")
download.file(url, destfile = temp_file, mode = "wb")
load(temp_file)

#  取得 a 小題模型的殘差
residuals_a <- resid(model_a)

#  設定畫布為 1列 2行，方便並排比較
par(mfrow = c(1, 2)) 

#  繪製殘差 vs income 
plot(vacation$income, residuals_a, 
     xlab = "income (所得)", ylab = "OLS 殘差", 
     main = "殘差 vs 所得", pch = 16, col = "blue")
# 畫一條 Y=0 的水平基準線方便觀察
abline(h = 0, col = "red", lty = 2, lwd = 2)

#  繪製殘差 vs age 
plot(vacation$age, residuals_a, 
     xlab = "age (年齡)", ylab = "OLS 殘差", 
     main = "殘差 vs 年齡", pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lty = 2, lwd = 2)


par(mfrow = c(1, 1))