#18.a

beta1_hat <- 6.855
beta2_hat <- 3.880
x_bar <- 59.3
y_bar <- beta1_hat + beta2_hat * x_bar
cat("y_bar=", y_bar, "\n")

#assume income in (0,120)
income_seq <- seq(0, 120, length.out = 100)
insurance_pred <- beta1_hat + beta2_hat * income_seq

plot(income_seq, insurance_pred, type = "l", col = "blue", lwd = 2,
     xlab = "Household Income (Thousands of $)",
     ylab = "Life Insurance Held (Thousands of $)",
     main = "Fitted Relationship",
     xlim=c(0,120), ylim=c(0,500))
     abline(h=0,v=0,col='black',lwd=1.5)

# Intercept
points(0, beta1_hat, col = "red", pch = 19)
text(0, beta1_hat, labels = paste0("Intercept(0, ", beta1_hat, ")"), pos = 4, cex = 0.8)

# (x_bar, y_bar)
points(x_bar, y_bar, col = "darkgreen", pch = 15, cex = 1.5)
text(x_bar, y_bar, labels = paste0("Mean(", x_bar, ", ", round(y_bar, 2), ")"), 
     pos = 3, cex = 0.9, font = 2)

# slope
text(80, 150, labels = paste0("Slope=", beta2_hat), col = "blue", font = 3)

#18.b

b2 <- 3.880
se_b2 <- 0.112
df <- 18

t_crit <- qt(0.975, df)

# CI
lower <- beta2 - t_crit * se_beta2
upper <- beta2 + t_crit * se_beta2

cat("95% CI for beta2: [", round(lower, 4), ",", round(upper, 4), "]\n")

#18.c

x0 <- 100
beta1_hat <- 6.855
beta2_hat <- 3.880
se_beta1 <- 7.383
se_beta2 <- 0.112
cov_b1b2 <- -0.746

# point estimate
y0_hat <- beta1_hat + beta2_hat * x0

var_y0 <- (se_beta1)^2 + (x0^2 * (se_beta2)^2) + (2 * x0 * cov_b1b2)
se_y0 <- sqrt(var_y0)

t_crit_99 <- qt(0.995, 18)

# CI
lower_99 <- y0_hat - t_crit_99 * se_y0
upper_99 <- y0_hat + t_crit_99 * se_y0

cat("Point Estimate at $100k:", y0_hat, "\n")
cat("Var: ", var_y0, "\n")
cat("99% Confidence Interval: [", round(lower_99, 4), ",", round(upper_99, 4), "]\n")

#d

beta2_hat <- 3.880
se_beta2 <- 0.112
beta2_null <- 5
t_stat <- (beta2_hat - beta2_null) / se_beta2
t_crit <- qt(0.975, 18)

cat("t-statistic:", t_stat, "\n")
cat("Critical value (5%):", t_crit, "\n")
cat("P-value:", 2 * pt(abs(t_stat), 18, lower.tail = FALSE), "\n")

#e

beta2_hat <- 3.880
se_beta2 <- 0.112
beta2_null_e <- 1
t_stat_e <- (beta2_hat - beta2_null_e) / se_beta2
#right tail
t_crit_e <- qt(0.99, 18)

cat("t-statistic:", t_stat_e, "\n")
cat("Critical value (1%, right-tail):", t_crit_e, "\n")

#23
url <- "http://www.principlesofeconometrics.com/poe5/data/csv/collegetown.csv"
df <- read.csv(url)
df$sqft2 <- (df$sqft)^2

#a
model_a <- lm(price ~ sqft2, data = df)
summary_a <- summary(model_a)

alpha2_hat <- coef(model_a)["sqft2"]
se_alpha2 <- summary_a$coefficients["sqft2", "Std. Error"]

# marginal effect
me_20 <- 40 * alpha2_hat
se_me_20 <- 40 * se_alpha2

# H0: 40*alpha2 = 13
t_stat <- (me_20 - 13) / se_me_20
t_crit_95 <- qt(0.95, 498)
df_deg <- df.residual(model_a)
p_val <- pt(t_stat, df_deg, lower.tail = FALSE)

cat("Marginal Effect at 2000 sqft:", me_20, "\n")
cat("t-statistic:", t_stat, "\n")
cat(t_crit_95)
cat("p-value:", p_val, "\n")

#b
model <- lm(price ~ sqft2, data = df)
alpha2_hat <- coef(model)["sqft2"]
se_alpha2 <- summary(model)$coefficients["sqft2", "Std. Error"]

#Marginal effect: SQFT = 40 
# ME = 2 * alpha2 * 40 = 80 * alpha2
me_40 <- 80 * alpha2_hat
se_me_40 <- 80 * se_alpha2

t_stat_b <- (me_40 - 13) / se_me_40
df_deg <- df.residual(model)
p_val_b <- pt(t_stat_b, df_deg, lower.tail = FALSE)

t_crit <- qt(0.95, df_deg)
cat("Estimated Marginal Effect:", me_40, "($13.0 means $13,000)\n")
cat("t-statistic:", t_stat_b, "\n")
cat("Critical value (t_c):", t_crit, "\n")
cat("p-value:", p_val_b, "\n")

#c
new_data <- data.frame(sqft2 = 400)
predict(model_a, new_data, interval = "confidence", level = 0.95)

#d
mean(df$price[df$sqft==20])

#31
url <- "http://www.principlesofeconometrics.com/poe5/data/csv/tuna.csv"
tuna <- read.csv(url)

#a
get_stats <- function(x) {
  c(Mean = mean(x), Min = min(x), Max = max(x), SD = sd(x))
}

sal_summary <- get_stats(tuna$sal1)
apr_summary <- get_stats(tuna$apr1)

print(sal_summary)
print(apr_summary)

#plot
par(mfrow = c(2, 1))
plot(tuna$WEEK, tuna$sal1, type = "l", col = "steelblue", lwd = 2,
     xlab = "Week", ylab = "Unit Sales (SAL1)",
     main = "Weekly Sales of Brand 1 Tuna")
plot(tuna$WEEK, tuna$apr1, type = "l", col = "lightpink", lwd = 2,
     xlab = "Week", ylab = "Price per Can (APR1)",
     main = "Weekly Price of Brand 1 Tuna")

#b

par(mfrow = c(1, 1))

plot(tuna$apr1, tuna$sal1, 
     pch = 19, 
     col = "darkgreen",
     xlab = "Price per Can (APR1)", 
     ylab = "Unit Sales (SAL1)",
     main = "Scatter Plot of Sales vs. Price")

abline(lm(sal1 ~ apr1, data = tuna), col = "red", lwd = 2)

#c

#variable: PRICE1 (100 * APR1)
tuna$PRICE1 <- 100 * tuna$apr1

model_c <- lm(sal1 ~ PRICE1, data = tuna)
summary_c <- summary(model_c)

print(summary_c)

conf_interval_c <- confint(model_c, "PRICE1", level = 0.95)
print(conf_interval_c)

#d
predict(model_c, data.frame(PRICE1 = 70), interval = "confidence", level = 0.90)

#e
beta2_hat <- coef(model_c)["PRICE1"]
se_beta2 <- summary(model_c)$coefficients["PRICE1", "Std. Error"]

avg_p <- mean(tuna$PRICE1)
avg_q <- mean(tuna$sal1)

elast_hat <- beta2_hat * (avg_p / avg_q)
se_elast <- se_beta2 * (avg_p / avg_q)

t_crit <- qt(0.975, 50)
lower_ci <- elast_hat - t_crit * se_elast
upper_ci <- elast_hat + t_crit * se_elast

cat("Elasticity Point Estimate:", elast_hat, "\n")
cat("95% Confidence Interval for Elasticity: [", lower_ci, ",", upper_ci, "]\n")

#f
null_epsilon <- -3
t_stat_f <- (elast_hat - null_epsilon) / se_elast

t_crit_f <- qt(0.95, 50)

# P-value
p_val_f <- 2 * pt(abs(t_stat_f), 50, lower.tail = FALSE)

cat("t-statistic for (f):", t_stat_f, "\n")
cat("Critical value (10%):", t_crit_f, "\n")
cat("P-value for (f):", p_val_f, "\n")
