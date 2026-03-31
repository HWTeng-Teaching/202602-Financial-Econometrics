library(POE5Rdata)
library(ggplot2)
library(tseries)
data("collegetown")
?data("collegetown")

#(a)
collegetown$ln_price <- log(collegetown$price)
model_loglinear <- lm(ln_price ~ sqft, data = collegetown)
summary(model_loglinear)

beta1 <- coef(model_loglinear)[1]
beta2 <- coef(model_loglinear)[2]

#sample mean
mean_sqft <- mean(collegetown$sqft)
cat("Sample mean for SQFT:", mean_sqft, "\n")
mean_price <- mean(collegetown$price)
cat("Sample mean for PRICE:", mean_price, "\n")

#slope at sample mean
# d(price)/d(sqft) = beta2 * price
slope_at_mean_loglinear <- beta2 * mean_price
cat("Slope at sample means:", slope_at_mean_loglinear, "\n")

#elasticity at sample mean
elasticity_at_mean_loglinear <- beta2 * mean_sqft
cat("Elasticity at sample means:", elasticity_at_mean_loglinear, "\n")

#(b)
collegetown$ln_sqft  <- log(collegetown$sqft)
model_loglog <- lm(ln_price ~ ln_sqft, data = collegetown)
summary(model_loglog)

alpha1 <- coef(model_loglog)[1]
alpha2 <- coef(model_loglog)[2]

#sample mean
mean_sqft <- mean(collegetown$sqft)
cat("Sample mean for SQFT:", mean_sqft, "\n")
mean_price <- mean(collegetown$price)
cat("Sample mean for PRICE:", mean_price, "\n")

#slope at sample means
# d(price)/d(sqft) = alpha2 * (price / sqft)
slope_at_mean_loglog <- alpha2 * (mean_price / mean_sqft)
cat("Slope at mean:", slope_at_mean_loglog, "\n")

#elasticity at sample means
elasticity_loglog <- alpha2
cat("Elasticity (alpha2):", elasticity_loglog, "\n")

#(c)
#log-linear model
log_linear = lm(log(price)~sqft,data = collegetown)
log_linear
sum_log_linear = summary(log_linear)
#R^2
sum_log_linear$r.squared

#general_R^2
y_hat = exp(coef(log_linear)[1]+coef(log_linear)[2]*collegetown$sqft)
y = collegetown$price
general_R = cor(y,y_hat)^2
general_R

#log-log model
log_log = lm(log(price)~log(sqft),data = collegetown)
log_log
sum_log_log = summary(log_log)
#R^2
sum_log_log$r.squared

#general_R^2
y_hat = exp(coef(log_log)[1]+coef(log_log)[2]*log(collegetown$sqft))
y = collegetown$price
general_R = cor(y,y_hat)^2
general_R

#linear model
linear = lm(price~sqft,data = collegetown)
linear
sumlinear = summary(linear)
#R^2
sumlinear$r.squared

#general_R^2
y_hat = coef(linear)[1]+coef(linear)[2]*collegetown$sqft
y = collegetown$price
general_R = cor(y,y_hat)^2
general_R

#(d)
#linear
model_linear <- lm(price ~ sqft, data = collegetown)
res_linear <- resid(model_linear)
hist(res_linear, main = "Residuals: Linear Model", xlab = "Residuals", breaks = 30)
jb_linear <- jarque.bera.test(res_linear)
jb_linear

#log-linear
model_loglin <- lm(log(price) ~ sqft, data = collegetown)
res_loglin <- resid(model_loglin)
hist(res_loglin, main = "Residuals: Log-Linear Model", xlab = "Residuals", breaks = 30)
jb_loglin <- jarque.bera.test(res_loglin)
jb_loglin

#log-log
model_loglog <- lm(log(price) ~ log(sqft), data = collegetown)
res_loglog <- resid(model_loglog)
hist(res_loglog, main = "Residuals: Log-Log Model", xlab = "Residuals", breaks = 30)
jb_loglog <- jarque.bera.test(res_loglog)
jb_loglog

#(e)
price = collegetown$price   
sqft = collegetown$sqft 

collegetown$ln_price <- log(price)
collegetown$ln_sqft  <- log(sqft)

model_loglinear <- lm(ln_price ~ sqft, data = collegetown)
model_loglog    <- lm(ln_price ~ ln_sqft, data = collegetown)
model_linear    <- lm(price ~ sqft, data = collegetown)

# residuals
a_residual = resid(model_loglinear)
b_residual = resid(model_loglog)
c_residual = resid(model_linear)

#log-linear
plot(sqft, a_residual,
     main = "Residuals against SQFT: Log-Linear",
     xlab = "SQFT",
     ylab = "Residuals",
     pch = 16,
     col = "steelblue",
     ylim = c(-200, 200))

#log-log
plot(sqft, b_residual,
     main = "Residuals against SQFT: Log-Log",
     xlab = "SQFT",
     ylab = "Residuals",
     pch = 16,
     col = "darkgreen",
     ylim = c(-200, 200))

#linear-linear
plot(sqft, c_residual,
     main = "Residuals against SQFT: Linear",
     xlab = "SQFT",
     ylab = "Residuals",
     pch = 16,
     col = "purple",
     ylim = c(-200, 200))

#(f)
sqft_f=2700/100
model_loglinear <- lm(ln_price ~ sqft, data = collegetown)
b1=coef(model_loglinear)[[1]]
b2=coef(model_loglinear)[[2]]
y1=exp(b1+b2*sqft_f)
cat("Predicted value for log-linear model:", y1*1000, "\n")

model_loglog    <- lm(ln_price ~ ln_sqft, data = collegetown)
a1=coef(model_loglog)[[1]]
a2=coef(model_loglog)[[2]]
y2=exp(a1+a2*log(sqft_f))
cat("Predicted value for log-log model:", y2*1000, "\n")

model_linear    <- lm(price ~ sqft, data = collegetown)
d1=coef(model_linear)[[1]]
d2=coef(model_linear)[[2]]
y3=d1+d2*sqft_f
cat("Predicted value for linear model:", y3*1000, "\n")

#(g)
sqft = collegetown$sqft
price = collegetown$price
ln_price = log(price)
ln_sqft = log(sqft)
mean_sqft = mean(sqft)
model_loglinear <- lm(ln_price ~ sqft, data = collegetown)
b1=coef(model_loglinear)[[1]]
b2=coef(model_loglinear)[[2]]
model_loglog    <- lm(ln_price ~ ln_sqft, data = collegetown)
a1=coef(model_loglog)[[1]]
a2=coef(model_loglog)[[2]]
model_linear    <- lm(price ~ sqft, data = collegetown)
d1=coef(model_linear)[[1]]
d2=coef(model_linear)[[2]]
price_1_hat = b1 + 27*b2
price_2_hat = a1 + log(27)*a2
price_3_hat = d1 + 27*d2

tvalue = qt(0.975,498)
vara_1_2 = vcov(model_loglinear)[2,2]
sm1 = summary(model_loglinear)
sigma_hat_1 = sm1$sigma^2 
varf_1 = sigma_hat_1 + sigma_hat_1/500 + (27- mean_sqft)^2 *vara_1_2 
sef_1 = sqrt(varf_1)
lowb_1 =exp(as.numeric(price_1_hat) - tvalue*sef_1)
upb_1 =exp(as.numeric(price_1_hat) + tvalue * sef_1)
cat("Prediction interval for log-linear model:", lowb_1,",",upb_1, "\n")

tvalue = qt(0.975,498)
vara_2_2 = vcov(model_loglog)[2,2]
sm2 = summary(model_loglog)
sigma_hat_2 = sm2$sigma^2 
varf_2 = sigma_hat_2 + sigma_hat_2/500 + (27- mean_sqft)^2 *vara_2_2 
sef_2 = sqrt(varf_2)
lowb_2 = exp(as.numeric(price_2_hat) - tvalue *sef_2)
upb_2 = exp(as.numeric(price_2_hat) + tvalue * sef_2)
cat("Prediction interval for log-log model:", lowb_2,",",upb_2, "\n")

tvalue = qt(0.975,498)
vara_3_2 = vcov(model_linear)[2,2]
sm3 = summary(model_linear)
sigma_hat_3 = sm3$sigma^2
varf_3 = sigma_hat_3 + sigma_hat_3/500 + (27- mean_sqft)^2 *vara_3_2
sef_3 = sqrt(varf_3)
lowb_3 = as.numeric(price_3_hat) - tvalue *sef_3
upb_3 = as.numeric(price_3_hat) + tvalue * sef_3
cat("Prediction interval for linear model:", lowb_3,",",upb_3, "\n")

#(h)
model_loglinear <- lm(ln_price ~ sqft, data = collegetown)
pred_loglinear <- predict(model_loglinear)
#prediction result
pred_loglinear <- c(pred_loglinear)

model_loglog <- lm(ln_price ~ ln_sqft, data = collegetown)
pred_loglog <- exp(predict(model_loglog))
pred_loglog <- c(pred_loglog)

model_linear <- lm(price ~ sqft, data = collegetown)
pred_linear <- predict(model_linear)
pred_linear <- c(pred_linear)

#actual value
actual <- c(collegetown$price)

#model RMSE
calculate_rmse <- function(pred, actual) {
  rmse <- sqrt(mean((pred - actual)^2))
  return(rmse)
}

#model RMSE
rmse_loglinear <- calculate_rmse(pred_loglinear, actual)
rmse_loglog <- calculate_rmse(pred_loglog, actual)
rmse_linear <- calculate_rmse(pred_linear, actual)
cat(paste("Log-linear model's RMSE:", rmse_loglinear, "\n"))
cat(paste("Log-log model's RMSE:", rmse_loglog, "\n"))
cat(paste("Linear model's RMSE:", rmse_linear, "\n"))

#model MSE
calculate_mse <- function(pred, actual) {
  mse <- mean((pred - actual)^2)
  return(mse)
}
mse_loglinear <- calculate_mse(pred_loglinear, actual)
mse_loglog <- calculate_mse(pred_loglog, actual)
mse_linear <- calculate_mse(pred_linear, actual)
cat(paste("Log-linear model's MSE:", mse_loglinear, "\n"))
cat(paste("Log-log model's MSE:", mse_loglog, "\n"))
cat(paste("Linear model's MSE:", mse_linear, "\n"))