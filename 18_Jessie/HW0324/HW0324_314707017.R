#4.25

rm(list=ls())
if (!require("devtools")) install.packages("devtools")
devtools::install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
install.packages("tseries")
library(tseries)
library(ggplot2)
head(collegetown)

# Models
m_linear <- lm(price ~ sqft, data = collegetown)
m_loglin <- lm(log(price) ~ sqft, data = collegetown)
m_loglog <- lm(log(price) ~ log(sqft), data = collegetown)
# Means
mean_price <- mean(collegetown$price)
mean_price
mean_sqft  <- mean(collegetown$sqft)
mean_sqft

# (a) log-linear
summary(m_loglin)
# Slope = beta2 * price_mean, Elasticity = beta2 * sqft_mean
b2 <- coef(m_loglin)[2]
slope_loglin <- b2 * mean_price
slope_loglin
elasticity_loglin <- b2 * mean_sqft
elasticity_loglin


#(b)
summary(m_loglog)
a2 <- coef(m_loglog)[2]
slope_loglog <- a2 * (mean_price / mean_sqft)
slope_loglog
# 在 log-log 模型中，係數 alpha2 即為彈性
elasticity_loglog <- a2
elasticity_loglog


#(c)
summary(m_linear)
gR2 <- function(y, yhat) cor(y, yhat)^2

gR2_linear <- gR2(collegetown$price, fitted(m_linear))
gR2_linear 
gR2_loglin <- gR2(collegetown$price, exp(fitted(m_loglin)))
gR2_loglin
gR2_loglog <- gR2(collegetown$price, exp(fitted(m_loglog)))
gR2_loglog


#(d)
hist(resid(m_linear), main="Linear residual")
jarque.bera.test(resid(m_linear))

hist(resid(m_loglin), main="Log-linear residual")
jarque.bera.test(resid(m_loglin))

hist(resid(m_loglog), main="Log-log residual")
jarque.bera.test(resid(m_loglog))


#(e)
plot(collegetown$sqft, resid(m_linear), main="Linear residual")
abline(h=0)
plot(collegetown$sqft, resid(m_loglin), main="Log-linear residual")
abline(h=0)
plot(collegetown$sqft, resid(m_loglog), main="Log-log residual")
abline(h=0)


#(f)
newdata <- data.frame(sqft = 27)

predict_linear <- predict(m_linear, newdata) #y=δ1+δ2*sqft
predict_linear
predict_loglin <- exp(predict(m_loglin, newdata))  #y=exp(b1+b2*sqft)
predict_loglin
predict_loglog <- exp(predict(m_loglog, newdata))  #y=exp(a1+a2*log(sqft))
predict_loglog



#(g)
PI_linear <- predict(m_linear, newdata, interval="prediction")
PI_linear
PI_loglin <- exp(predict(m_loglin, newdata, interval="prediction"))
PI_loglin
PI_loglog <- exp(predict(m_loglog, newdata, interval="prediction"))
PI_loglog




# (h)
# 真實值
# actual <- c(collegetown$price)

# 計算均方根誤差 (RMSE) 函數
# calculate_rmse <- function(pred, actual) {
#   rmse <- sqrt(mean((pred - actual)^2))
#   return(rmse)
# }

# 計算每個模型的 RMSE
# rmse_log_linear <- calculate_rmse(pred_log_linear, actual)
# rmse_log_log <- calculate_rmse(pred_log_log, actual)
# rmse_linear <- calculate_rmse(pred_linear, actual)

# 輸出結果
# cat(paste("Log-linear model's RMSE:", rmse_log_linear))
# cat('\n')
# cat(paste("Log-log model's RMSE:", rmse_log_log))
# cat('\n')
# cat(paste("Linear model's RMSE:", rmse_linear))

# 計算均方根誤差 (MSE) 函數
# calculate_mse <- function(pred, actual) {
#   mse <- mean((pred - actual)^2)
#   return(mse)
# }

# 計算每個模型的 MSE
# mse_log_linear <- calculate_mse(pred_log_linear, actual)
# mse_log_log <- calculate_mse(pred_log_log, actual)
# mse_linear <- calculate_mse(pred_linear, actual)

# 輸出結果
# cat(paste("Log-linear model's MSE:", mse_log_linear))
# cat('\n')
# cat(paste("Log-log model's MSE:", mse_log_log))
# cat('\n')
# cat(paste("Linear model's MSE:", mse_linear))

