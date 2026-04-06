url <- "http://www.principlesofeconometrics.com/poe5/data/csv/collegetown.csv"
df <- read.csv(url)


# 4.25a
model_log_linear <- lm(log(price) ~ sqft, data = df)
avg_price <- mean(df$price)
avg_sqft <- mean(df$sqft)
beta2 <- coef(model_log_linear)[2]
summary_a <- summary(model_log_linear)
print(summary_a)
avg_price*beta2
avg_sqft*beta2

#b
model_log_log <- lm(log(price) ~ log(sqft), data = df)
avg_price <- mean(df$price)
avg_sqft <- mean(df$sqft)
alpha2 <- coef(model_log_log)[2]
summary_b <- summary(model_log_log)
print(summary_b)
alpha2*avg_price/avg_sqft
alpha2

#c
#Linear Model
model_linear <- lm(price ~ sqft, data = df)
r2_linear <- summary(model_linear)$r.squared

#Log-Linear
pred_log_linear <- exp(predict(model_log_linear))
r2_gen_a <- cor(df$price, pred_log_linear)^2

# Log-Log 
pred_log_log <- exp(predict(model_log_log))
r2_gen_b <- cor(df$price, pred_log_log)^2

r2_linear
r2_gen_a
r2_gen_b

#d
library(tseries) 

# residual
res_lin <- residuals(model_linear)
res_a   <- residuals(model_log_linear)
res_b   <- residuals(model_log_log)

par(mfrow = c(1, 3))

hist(res_lin, main="Linear Residuals", breaks=20)
hist(res_a,   main="Log-Linear Residuals", breaks=20)
hist(res_b,   main="Log-Log Residuals", breaks=20)

jb_lin <- jarque.bera.test(res_lin)
jb_a   <- jarque.bera.test(res_a)
jb_b   <- jarque.bera.test(res_b)
list(Linear=jb_lin, Log_Linear=jb_a, Log_Log=jb_b)

#e
par(mfrow = c(3, 1))

plot(df$sqft, res_lin, main="Linear vs SQFT", col="steelblue", pch=20)
abline(h=0, col="red", lwd=2) 

plot(df$sqft, res_a, main="Log-Linear vs SQFT", col="darkorange", pch=20)
abline(h=0, col="red", lwd=2)

plot(df$sqft, res_b, main="Log-Log vs SQFT", col="darkgreen", pch=20)
abline(h=0, col="red", lwd=2)


new_h <- data.frame(sqft = 27)
# f
p_lin <- predict(model_linear, new_h)
p_a   <- exp(predict(model_log_linear, new_h))
p_b   <- exp(predict(model_log_log, new_h))

#g
pi_lin <- predict(model_linear, new_h, interval = "prediction", level = 0.95)
pi_a   <- exp(predict(model_log_linear, new_h, interval = "prediction", level = 0.95))
pi_b   <- exp(predict(model_log_log, new_h, interval = "prediction", level = 0.95))

print(list(Linear=pi_lin, Log_Linear=pi_a, Log_Log=pi_b))

#5.20
sigma2 <- 1000
N_list <- c(100, 500, 1000, 5000)

results <- data.frame()

for (N in N_list) {
  x <- sort(runif(N, 0, 10))
  
  x1 <- x[1:(N/2)]
  x2 <- x[(N/2 + 1):N]
  
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  sx2 <- sum((x - mean(x))^2) / N 
  
  var_ols <- sigma2 / (N * sx2)
  var_mean <- (4 * sigma2) / (N * (mean_x2 - mean_x1)^2)
  
  inv_sx2 <- 1 / sx2
  term_mean <- 4 / (mean_x2 - mean_x1)^2
  
  results <- rbind(results, data.frame(N=N, var_ols=var_ols, var_mean=var_mean, 
                                       inv_sx2=inv_sx2, term_mean=term_mean))
}

print(results)