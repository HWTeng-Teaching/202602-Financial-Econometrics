library(stargazer)
library(tidyverse)
library(POE5Rdata)

data(collegetown)

# 4.25
# (a)
model_1a <- lm(log(price) ~ sqft ,data = collegetown)
smodel_1a <- summary(model_1a)

beta_1a <- coef(model_1a)["sqft"]

avg_sqft <- mean(collegetown$sqft)
avg_prc <- mean(collegetown$price)

slope_1a <- beta_1a * avg_prc
elas_1a <- beta_1a * avg_sqft

result <- data.frame(
  "值" = c(slope_1a, elas_1a),
  row.names = c("slope", "elasticity")
)

stargazer(result, type='text', summary = FALSE)


# (b)
model_1b <- lm(log(price) ~ log(sqft), data = collegetown)
smodel_1b <- summary(model_1b)

alpha2 <- coef(model_1b)["log(sqft)"]

avg_sqft <- mean(collegetown$sqft)
avg_prc <- mean(collegetown$price)

elas_1b <- alpha2

slope_1b <- alpha2 * (avg_prc / avg_sqft)

result_b <- data.frame(
  "值" = c(slope_1b, elas_1b),
  row.names = c("slope", "elasticity")
)

stargazer(result_b, type='text', summary = FALSE)

# (c)
model_linear <- lm(price ~ sqft, data = collegetown)
r2_lin <- summary(model_linear)$r.squared

pred_a <- exp(predict(model_1a))
r2_gen_a <- cor(collegetown$price, pred_a)^2

pred_b <- exp(predict(model_1b))
r2_gen_b <- cor(collegetown$price, pred_b)^2

comparison <- data.frame(
  "R_squared_Type" = c("Standard R2", "Generalized R2", "Generalized R2"),
  "Value" = c(r2_lin, r2_gen_a, r2_gen_b),
  row.names = c("Linear Model", "Log-Lin (a)", "Log-Log (b)")
)

stargazer(comparison, type='text', summary = FALSE)

# (d)
library(tseries)

par(mfrow = c(1, 3))

hist(residuals(model_linear), breaks = 20, 
     main = "Linear Model Residuals", 
     xlab = "Residuals", col = "gray80", border = "white")

hist(residuals(model_1a), breaks = 20, 
     main = "Log-Lin Model Residuals", 
     xlab = "Residuals", col = "lightblue", border = "white")

hist(residuals(model_1b), breaks = 20, 
     main = "Log-Log Model Residuals", 
     xlab = "Residuals", col = "lightgreen", border = "white")

par(mfrow = c(1, 1))

jb_results <- data.frame(
  "JB_Stat" = c(jarque.bera.test(residuals(model_linear))$statistic,
                jarque.bera.test(residuals(model_1a))$statistic,
                jarque.bera.test(residuals(model_1b))$statistic),
  "p_value" = c(jarque.bera.test(residuals(model_linear))$p.value,
                jarque.bera.test(residuals(model_1a))$p.value,
                jarque.bera.test(residuals(model_1b))$p.value),
  row.names = c("Linear", "Log-Lin", "Log-Log")
)

stargazer(jb_results, type='text', summary = FALSE)

# (e)
par(mfrow = c(1, 3))

plot(collegetown$sqft, residuals(model_linear), 
     main = "Linear: Residuals vs SQFT", 
     xlab = "SQFT", ylab = "Residuals", col = "gray")
abline(h = 0, col = "red", lwd = 2)

plot(collegetown$sqft, residuals(model_1a), 
     main = "Log-Lin: Residuals vs SQFT", 
     xlab = "SQFT", ylab = "Residuals", col = "lightblue")
abline(h = 0, col = "red", lwd = 2)

plot(collegetown$sqft, residuals(model_1b), 
     main = "Log-Log: Residuals vs SQFT", 
     xlab = "SQFT", ylab = "Residuals", col = "lightgreen")
abline(h = 0, col = "red", lwd = 2)

par(mfrow = c(1, 1))


# (f)
new_data <- data.frame(sqft = 27)

pred_lin <- predict(model_linear, new_data)

pred_a_log <- predict(model_1a, new_data)
pred_a <- exp(pred_a_log)

pred_b_log <- predict(model_1b, new_data)
pred_b <- exp(pred_b_log)

prediction_results <- data.frame(
  "Model" = c("Linear", "Log-Lin", "Log-Log"),
  "Predicted_Price" = c(pred_lin, pred_a, pred_b)
)

stargazer(prediction_results, type='text', summary = FALSE)

# (g)
new_data <- data.frame(sqft = 27)

pi_lin <- predict(model_linear, new_data, interval = "prediction", level = 0.95)

pi_a_log <- predict(model_1a, new_data, interval = "prediction", level = 0.95)
pi_a <- exp(pi_a_log)

pi_b_log <- predict(model_1b, new_data, interval = "prediction", level = 0.95)
pi_b <- exp(pi_b_log)

prediction_intervals <- data.frame(
  "Model" = c("Linear", "Log-Lin", "Log-Log"),
  "Fit"   = c(pi_lin[1], pi_a[1], pi_b[1]),
  "Lower" = c(pi_lin[2], pi_a[2], pi_b[2]),
  "Upper" = c(pi_lin[3], pi_a[3], pi_b[3])
)

stargazer(prediction_intervals, type='text', summary = FALSE)

# 5.20
# (e)
set.seed(123)
sigma2 <- 1000
Ns <- c(100, 500, 1000, 5000)
R <- 1000

summary_results <- data.frame(
  "N=100"  = numeric(4),
  "N=500"  = numeric(4),
  "N=1000" = numeric(4),
  "N=5000" = numeric(4),
  row.names = c("Mean_var_ols", "Mean_var_mean", "E[1/sx2]", "Ex_var")
)

for (i in 1:length(Ns)) {
  N <- Ns[i]
  
  var_ols <- numeric(R)
  var_mean <- numeric(R)
  inv_sx2 <- numeric(R)
  inv_diff <- numeric(R)
  
  for (r in 1:R) {
    x <- runif(N, 0, 10)
    sx2 <- mean((x - mean(x))^2)
    
    var_ols[r] <- sigma2 / (N * sx2)
    
    x_sorted <- sort(x)
    x1 <- x_sorted[1:(N/2)]
    x2 <- x_sorted[(N/2 + 1):N]
    
    xbar1 <- mean(x1)
    xbar2 <- mean(x2)
    
    var_mean[r] <- (4 * sigma2 / N) / ((xbar2 - xbar1)^2)
    
    inv_sx2[r] <- 1 / sx2
    inv_diff[r] <- 4 / (xbar2 - xbar1)^2
  }
  
  summary_results[, i] <- c(
    mean(var_ols),
    mean(var_mean),
    mean(inv_sx2),
    mean(inv_diff)
  )
}