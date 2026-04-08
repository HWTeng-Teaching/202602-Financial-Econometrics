rm(list=ls())
library(POE5Rdata)
data(collegetown)

# a.
model_a = lm(log(price)~sqft,data=collegetown)
summary(model_a)
beta2_a = coef(model_a)['sqft']

mean_price = mean(collegetown$price,na.rm=TRUE)
mean_sqft = mean(collegetown$sqft,na.rm=TRUE)

slope_at_mean_a = beta2_a*mean_price
elasticity_at_mean_a = beta2_a*mean_sqft

cat("Slope at sample mean:",slope_at_mean_a,"\n")
cat("Elasticity at sample mean",elasticity_at_mean_a,"\n")

# b.
model_b = lm(log(price)~log(sqft),data=collegetown)
summary(model_b)
beta2_b = coef(model_b)['log(sqft)']

elasticity_at_mean_b = beta2_b
slope_at_mean_b = beta2_b*(mean_price/mean_sqft)

cat("Slope at sample mean:",slope_at_mean_b,"\n")
cat("Elasticity at sample mean",elasticity_at_mean_b,"\n")

 
# c.

model_c = lm(price~sqft,data=collegetown)
r2_linear = summary(model_c)$r.squared

pred_log_price_a = predict(model_a)
pred_price_a = exp(pred_log_price_a)
r2_gen_a = cor(collegetown$price,pred_price_a)^2

pred_log_price_b = predict(model_b)
pred_price_b = exp(pred_log_price_b)
r2_gen_b = cor(collegetown$price, pred_price_b)^2

cat("Linear Model R-squared:", r2_linear, "\n")
cat("Generalized R-squared for Log-Linear (a):", r2_gen_a, "\n")
cat("Generalized R-squared for Log-Log (b):", r2_gen_b, "\n")


# d.
res_a = residuals(model_a)
res_b = residuals(model_b)
res_c = residuals(model_c)

hist(res_a,main="log-linear model", xlab = "Residuals", col = "lightblue", breaks = 20)
hist(res_b,main='log-log model', xlab = "Residuals", col = "lightblue", breaks = 20)
hist(res_c,main="linear model", xlab = "Residuals", col = "lightblue", breaks = 20)

jb_a = jarque.bera.test(res_a)
jb_b = jarque.bera.test(res_b)
jb_c = jarque.bera.test(res_c)

cat("JB test(a):",'\n')
print(jb_a)
cat("JB test(b):",'\n')
print(jb_b)
cat("JB test(c):",'\n')
print(jb_c)


# e.

plot(collegetown$sqft, res_a, 
     main = "Log-Linear Model", 
     xlab = "SQFT", ylab = "Residuals", 
     col = "blue", pch = 16, cex = 0.7)
abline(h = 0, col = "red", lwd = 2, lty = 2) 


plot(collegetown$sqft, res_b, 
     main = "Log-Log Model", 
     xlab = "SQFT", ylab = "Residuals", 
     col = "darkgreen", pch = 16, cex = 0.7)
abline(h = 0, col = "red", lwd = 2, lty = 2)


plot(collegetown$sqft, res_c, 
     main = "Linear Model", 
     xlab = "SQFT", ylab = "Residuals", 
     col = "darkorange", pch = 16, cex = 0.7)
abline(h = 0, col = "red", lwd = 2, lty = 2)


# f.
new_house = data.frame(sqft=27)

predict_log_a = predict(model_a,newdata=new_house)
price_a_naive = exp(predict_log_a)
sigma2_a = (summary(model_a)$sigma)^2
price_a_corrected = exp(predict_log_a+sigma2_a/2)


predict_log_b = predict(model_b,newdata=new_house)
price_b_naive = exp(predict_log_b)
sigma2_b = (summary(model_b)$sigma)^2
price_b_corrected = exp(predict_log_b+(sigma2_b/2))

price_c = predict(model_c,newdata=new_house)

cat("--- Predictions for a 2700 SQFT House ---\n")
cat("Model (a) Log-Linear (Naive):", price_a_naive, "\n")
cat("Model (a) Log-Linear (Corrected):", price_a_corrected, "\n\n")

cat("Model (b) Log-Log (Naive):", price_b_naive, "\n")
cat("Model (b) Log-Log (Corrected):", price_b_corrected, "\n\n")

cat("Model (c) Linear:", price_c, "\n")


# g.

log_pred_a = predict(model_a, newdata = new_house, interval = "prediction", level = 0.95)

pred_a = exp(log_pred_a)

cat("\n--- Model (a) Log-Linear 95% Prediction Interval ---\n")
print(pred_a)

log_pred_b = predict(model_b, newdata = new_house, interval = "prediction", level = 0.95)

pred_b <- exp(log_pred_b)

cat("\n--- Model (b) Log-Log 95% Prediction Interval ---\n")
print(pred_b)


pred_c = predict(model_c, newdata = new_house, interval = "prediction", level = 0.95)

cat("--- Model (c) Linear 95% Prediction Interval ---\n")
print(pred_c)




