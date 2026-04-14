# 載入必要的套件
library(tseries) # 用於 Jarque-Bera 檢定
library(ggplot2) # 用於繪圖
library(xtable)
library(knitr)

#a
mod1 <- lm(log(price)~sqft, data=collegetown)
summary1<-summary(mod1)
b1 <- coef(mod1)[1]
b2 <- coef(mod1)[2]
mean_price <- mean(collegetown$price)
mean_sqft <- mean(collegetown$sqft)
slope1 <- b2*mean_price
elasticity1 <- b2*mean_sqft
table<- data.frame(slope1, elasticity1)
kable(table)
table<- data.frame(xtable(mod1))
kable(table, caption = "Regression output showing the coefficients")

#b
mod2 <- lm(log(price)~log(sqft), data=collegetown)
summary2<-summary(mod2)
a1 <- coef(mod2)[1]
a2 <- coef(mod2)[2]
slope2 <- a2*mean_price/mean_sqft
elasticity2 <- a2
table<- data.frame(slope2, elasticity2)
kable(table)
table<- data.frame(xtable(mod2))
kable(table, caption = "Regression output showing the coefficients")

#c
#Linear Model (用於比較)
model_c_lin <- lm(price ~ sqft, data = collegetown)
r2_linear <- summary(model_c_lin)$r.squared

# 計算 Generalized R-squared (對數模型預測值與實際 PRICE 的相關係數平方)
pred_price_1 <- exp(predict(mod1) + var(resid(mod1))/2)
pred_price_2 <- exp(predict(mod2) + var(resid(mod2))/2)
gen_r2_1 <- cor(collegetown$price, pred_price_1)^2
gen_r2_2 <- cor(collegetown$price, pred_price_2)^2
table<- data.frame(gen_r2_1, gen_r2_2,r2_linear)
kable(table, caption = "Generalized R^2")

#d
res_1 <- resid(mod1)
res_2 <- resid(mod2)
res_c <- resid(model_c_lin)
# 設定畫布為 1 列 3 欄，方便並排比較三個模型的殘差形狀
par(mfrow = c(1, 3))
hist(res_1, main = "Residuals: Log-Linear (a)",
     xlab = "Residuals", col = "lightblue", breaks = 20)
hist(res_2, main = "Residuals: Log-Log (b)",
     xlab = "Residuals", col = "lightgreen", breaks = 20)
hist(res_c, main = "Residuals: Linear (c)",
     xlab = "Residuals", col = "salmon", breaks = 20)
# 恢復畫布為預設的 1 列 1 欄
par(mfrow = c(1, 1))

jb_1 <- jarque.bera.test(resid(mod1))
jb_2 <- jarque.bera.test(resid(mod2))
jb_c <- jarque.bera.test(resid(model_c_lin))
print("--- Jarque-Bera Test: Model (a) Log-Linear ---")
print(jb_1)
print("--- Jarque-Bera Test: Model (b) Log-Log ---")
print(jb_2)
print("--- Jarque-Bera Test: Model (c) Linear ---")
print(jb_c)

#e
# 提取 SQFT 變數
sqft<- collegetown$sqft
par(mfrow = c(1, 3))
# (a) Log-Linear 殘差圖
plot(sqft, res_1, main = "Residuals vs SQFT: (a) Log-Linear",
     xlab = "SQFT", ylab = "Residuals", col = "blue", pch = 20)
abline(h = 0, col = "red", lwd = 2, lty = 2) # 加入 y=0 的紅色虛線

# (b) Log-Log 殘差圖
plot(sqft, res_2, main = "Residuals vs SQFT: (b) Log-Log",
     xlab = "SQFT", ylab = "Residuals", col = "green", pch = 20)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# (c) Linear 殘差圖
plot(sqft, res_c, main = "Residuals vs SQFT: (c) Linear",
     xlab = "SQFT", ylab = "Residuals", col = "black", pch = 20)
abline(h = 0, col = "red", lwd = 2, lty = 2)
par(mfrow = c(1, 1))

#f
new_data <- data.frame(sqft = 27)
# (c) Linear Model 預測
# 線性模型不需要反對數轉換，直接預測即為 PRICE
pred_c <- predict(model_c_lin, newdata = new_data)
# (a) Log-Linear Model 預測
# 第一步：得出 ln(PRICE) 的預測值
pred_log_a <- predict(mod1, newdata = new_data)
# 第二步：取指數轉回原始 PRICE (簡單轉換)
pred_a_simple <- exp(pred_log_a)
# (進階) 偏差修正：在計量經濟學中，嚴謹的做法會加上殘差變異數的一半來進行修正
var_res_a <- (summary(mod1)$sigma)^2 
pred_a_corrected <- exp(pred_log_a + var_res_a / 2)
# (b) Log-Log Model 預測
pred_log_b <- predict(mod2, newdata = new_data)
pred_b_simple <- exp(pred_log_b)
var_res_b <- (summary(mod2)$sigma)^2
pred_b_corrected <- exp(pred_log_b + var_res_b / 2)
results_table <- data.frame(
  Model = c("Linear", "Log-Linear", "Log-Log"),
  
  # 填入 Raw exp(y_hat) 的數值
  `Raw exp(y_hat)` = c(pred_c, pred_a_simple, pred_b_simple),
  
  # 填入 Corrected Forecast 的數值
  `Corrected Forecast` = c(pred_c, pred_a_corrected, pred_b_corrected),
  
  # 設定 check.names = FALSE，這樣 R 就不會自動把我們欄位名稱裡的空格跟括號變成底線或句號
  check.names = FALSE 
)  
results_table$`Raw exp(y_hat)` <- round(results_table$`Raw exp(y_hat)`, 2)
results_table$`Corrected Forecast` <- round(results_table$`Corrected Forecast`, 2)

print("--- 在 Console 顯示的簡單表格 ---")
print(results_table)

#g
# (c) Linear Model (線性模型不需轉換，直接得出 PRICE 的區間)
pi_c <- predict(model_c_lin, newdata = new_data, interval = "prediction", level = 0.95)
# (a) Log-Linear Model (先算出 ln(PRICE) 的區間，再取 exp 轉回 PRICE)
pi_log_a <- predict(mod1, newdata = new_data, interval = "prediction", level = 0.95)
pi_a <- exp(pi_log_a)
# (b) Log-Log Model (先算出 ln(PRICE) 的區間，再取 exp 轉回 PRICE)
pi_log_b <- predict(mod2, newdata = new_data, interval = "prediction", level = 0.95)
pi_b <- exp(pi_log_b)
results_pi <- data.frame(
  Model = c("Linear", "Log-Linear", "Log-Log"),
  
  # 提取下界 (lwr 位於矩陣的第 2 欄)
  `Lower Bound` = c(pi_c[, "lwr"], pi_a[, "lwr"], pi_b[, "lwr"]),
  
  # 提取上界 (upr 位於矩陣的第 3 欄)
  `Upper Bound` = c(pi_c[, "upr"], pi_a[, "upr"], pi_b[, "upr"]),
  
  check.names = FALSE
)
results_pi$`Lower Bound` <- round(results_pi$`Lower Bound`, 2)
results_pi$`Upper Bound` <- round(results_pi$`Upper Bound`, 2)
print("--- 95% Prediction Intervals for SQFT = 2700 ---")
print(results_pi)
