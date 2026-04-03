library(POE5Rdata)
library(tseries)
data("collegetown")

# setup
model_loglinear <- lm(log(price) ~ sqft,       data = collegetown)
model_loglog    <- lm(log(price) ~ log(sqft),  data = collegetown)
model_linear    <- lm(price ~ sqft,            data = collegetown)

mean_sqft  <- mean(collegetown$sqft)
mean_price <- mean(collegetown$price)

#(a)
summary(model_loglinear)
beta1 <- coef(model_loglinear)[1]
beta2 <- coef(model_loglinear)[2]

cat("Sample mean SQFT:", mean_sqft, "\n")
cat("Sample mean PRICE:", mean_price, "\n")
cat("Slope at means:", beta2 * mean_price, "\n")
cat("Elasticity at means:", beta2 * mean_sqft, "\n")

#(b)
summary(model_loglog)
alpha1 <- coef(model_loglog)[1]
alpha2 <- coef(model_loglog)[2]

cat("Slope at means:", alpha2 * (mean_price / mean_sqft), "\n")
cat("Elasticity (alpha2):", alpha2, "\n")

#(c)
# R^2
cat("Log-linear R^2:", summary(model_loglinear)$r.squared, "\n")
cat("Log-log R^2:",    summary(model_loglog)$r.squared, "\n")
cat("Linear R^2:",     summary(model_linear)$r.squared, "\n")

# Generalized R^2
gen_r2 <- function(model, log_y = TRUE) {
  y <- collegetown$price
  y_hat <- if (log_y) exp(fitted(model)) else fitted(model)
  cor(y, y_hat)^2
}
cat("Log-linear Rg^2:", gen_r2(model_loglinear), "\n")
cat("Log-log Rg^2:",    gen_r2(model_loglog), "\n")
cat("Linear Rg^2:",     gen_r2(model_linear, log_y = FALSE), "\n")

#(d)
res_loglinear <- resid(model_loglinear)
res_loglog    <- resid(model_loglog)
res_linear    <- resid(model_linear)

hist(res_loglinear, main = "Residuals: Log-Linear", xlab = "Residuals", breaks = 30)
hist(res_loglog,    main = "Residuals: Log-Log",    xlab = "Residuals", breaks = 30)
hist(res_linear,    main = "Residuals: Linear",     xlab = "Residuals", breaks = 30)

jarque.bera.test(res_loglinear)
jarque.bera.test(res_loglog)
jarque.bera.test(res_linear)

#(e)
plot(collegetown$sqft, res_loglinear,
     main = "Residuals vs SQFT: Log-Linear",
     xlab = "SQFT", ylab = "Residuals", pch = 16, col = "steelblue")

plot(collegetown$sqft, res_loglog,
     main = "Residuals vs SQFT: Log-Log",
     xlab = "SQFT", ylab = "Residuals", pch = 16, col = "darkgreen")

plot(collegetown$sqft, res_linear,
     main = "Residuals vs SQFT: Linear",
     xlab = "SQFT", ylab = "Residuals", pch = 16, col = "purple")

#(f)
sqft_f <- 27

y1 <- exp(coef(model_loglinear)[1] + coef(model_loglinear)[2] * sqft_f)
y2 <- exp(coef(model_loglog)[1]    + coef(model_loglog)[2]    * log(sqft_f))
y3 <- coef(model_linear)[1]        + coef(model_linear)[2]    * sqft_f

cat("Log-linear prediction:", y1 * 1000, "\n")
cat("Log-log prediction:",    y2 * 1000, "\n")
cat("Linear prediction:",     y3 * 1000, "\n")

#(g)
tval <- qt(0.975, 498)

pred_interval <- function(model, x0, log_y = TRUE, log_x = FALSE) {
  x_val  <- if (log_x) log(x0) else x0
  x_mean <- if (log_x) mean(log(collegetown$sqft)) else mean_sqft
  y_hat  <- coef(model)[1] + coef(model)[2] * x_val
  sig2   <- summary(model)$sigma^2
  var_b2 <- vcov(model)[2, 2]
  se_f   <- sqrt(sig2 + sig2 / 500 + (x_val - x_mean)^2 * var_b2)
  if (log_y) {
    c(exp(y_hat - tval * se_f), exp(y_hat + tval * se_f)) * 1000
  } else {
    c(y_hat - tval * se_f, y_hat + tval * se_f) * 1000
  }
}

cat("Log-linear PI:", pred_interval(model_loglinear, sqft_f), "\n")
cat("Log-log PI:",    pred_interval(model_loglog,    sqft_f, log_y = TRUE, log_x = TRUE), "\n")
cat("Linear PI:",     pred_interval(model_linear,    sqft_f, log_y = FALSE), "\n")
