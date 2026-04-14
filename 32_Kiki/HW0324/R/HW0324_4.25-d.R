
rm(list=ls())


library(tseries) 


data_url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata" 
load(url(data_url))


collegetown$ln_price <- log(collegetown$price)
collegetown$ln_sqft <- log(collegetown$sqft)

# 建立模型 
model_A <- lm(ln_price ~ sqft, data = collegetown)
model_B <- lm(ln_price ~ ln_sqft, data = collegetown)
model_C <- lm(price ~ sqft, data = collegetown)

# 取得殘差
res_A <- residuals(model_A)
res_B <- residuals(model_B)
res_C <- residuals(model_C)

# 畫出三個模型的殘差直方圖 
par(mfrow=c(1,3))
hist(res_A, main="Log-Lin Residuals", xlab="Residuals", col="lightblue")
hist(res_B, main="Log-Log Residuals", xlab="Residuals", col="lightgreen")
hist(res_C, main="Linear Residuals", xlab="Residuals", col="lightcoral")

# Jarque-Bera Test
jb_A <- jarque.bera.test(res_A)
jb_B <- jarque.bera.test(res_B)
jb_C <- jarque.bera.test(res_C)


cat("\n--- (d) Jarque-Bera p-values (H0: 常態分配) ---\n")
cat("Log-Lin:", jb_A$p.value, "\nLog-Log:", jb_B$p.value, "\nLinear:", jb_C$p.value, "\n")
# 【結論判斷】：如果 p-value < 0.05，代表拒絕常態分配的假設。


par(mfrow=c(1,1))