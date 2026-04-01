# 4.25

rm(list = ls())
library(POE5Rdata)
library(tseries) # For Jarque-Bera test
library(knitr)
library(ggplot2)

data(collegetown)
summary(collegetown)

avg_sqft <- mean(collegetown$sqft)
avg_price <- mean(collegetown$price)

# (a) Log-Linear Model
mod_a <- lm(log(price) ~ sqft, data = collegetown)
summary(mod_a)

b2 <- coef(mod_a)["sqft"] # beta2

# Slope: dP/dx = beta2 * P
# Elasticity: epsilon = beta2 * x
slope_a <- b2 * avg_price
elasticity_a <- b2 * avg_sqft

# (b) Log-Log Model 
mod_b <- lm(log(price) ~ log(sqft), data = collegetown)
summary(mod_b)

a2 <- coef(mod_b)["log(sqft)"] #alpha2

# Slope: dP/dx = alpha2 * (P/x)
# Elasticity: alpha2
slope_b <- a2 * (avg_price / avg_sqft)
elasticity_b <- a2

# (c) Linear Model
mod_c <- lm(price ~ sqft, data = collegetown)
summary(mod_c)

# Generalized R-squared (For Log Model Comparison)
y_true <- collegetown$price
gen_r2_a <- cor(y_true, exp(predict(mod_a)))^2
gen_r2_b <- cor(y_true, exp(predict(mod_b)))^2
r2_c <- summary(mod_c)$r.squared

# (d) Normality Check (Histogram + Residual Plot + JB Test)
# Histograms for Normality Check
hist(residuals(mod_a), breaks = 20, col = "skyblue", main = "Log-Linear Residuals", xlab = "Residuals")
hist(residuals(mod_b), breaks = 20, col = "lightgreen", main = "Log-Log Residuals", xlab = "Residuals")
hist(residuals(mod_c), breaks = 20, col = "salmon", main = "Linear Residuals", xlab = "Residuals")

# Residual Plots (Residuals vs SQFT)
plot(collegetown$sqft, residuals(mod_a), main = "Log-Linear: Residual vs SQFT", xlab = "SQFT", ylab = "Residuals")
abline(h = 0, col = "red")
plot(collegetown$sqft, residuals(mod_b), main = "Log-Log: Residual vs SQFT", xlab = "SQFT", ylab = "Residuals")
abline(h = 0, col = "red")
plot(collegetown$sqft, residuals(mod_c), main = "Linear: Residual vs SQFT", xlab = "SQFT", ylab = "Residuals")
abline(h = 0, col = "red")

jb_a <- jarque.bera.test(residuals(mod_a))
jb_b <- jarque.bera.test(residuals(mod_b))
jb_c <- jarque.bera.test(residuals(mod_c))

# --- (f) & (g) Prediction at 2700 sqft ---
new_data <- data.frame(sqft = 27) 

# log(price) ---exp()---> price
pred_a_log <- predict(mod_a, new_data, interval = "prediction", level = 0.95)
pred_a <- exp(pred_a_log) 

pred_b_log <- predict(mod_b, new_data, interval = "prediction", level = 0.95)
pred_b <- exp(pred_b_log)

pred_c <- predict(mod_c, new_data, interval = "prediction", level = 0.95)

# Summary Table
results_comparison <- data.frame(
  Model = c("Log-Linear", "Log-Log", "Linear"),
  R_squared = c(gen_r2_a, gen_r2_b, r2_c),
  JB_p_value = c(jb_a$p.value, jb_b$p.value, jb_c$p.value),
  Pred_Price_2700 = c(pred_a[1], pred_b[1], pred_c[1])
)

kable(results_comparison, caption = "Comparison of Model Results and Predictions", digits = 4)