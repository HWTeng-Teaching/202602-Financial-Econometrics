# Chan Nok Hang 414707007
#hw2 ch3-Q31
# Load the data
library(POE5Rdata)
data(tuna)

# ==========================================
# PART A: Summary Statistics and Time Plots
# ==========================================

# 1. Summary stats for SAL1 (Sales)
cat("--- Summary for SAL1 ---\n")
summary(tuna$sal1)
cat("Std Dev of SAL1:", sd(tuna$sal1), "\n\n")

# 2. Summary stats for APR1 (Price in dollars)
cat("--- Summary for APR1 ---\n")
summary(tuna$apr1)
cat("Std Dev of APR1:", sd(tuna$apr1), "\n\n")

# 3. Time series plots
tuna$week <- 1:nrow(tuna)
# Plot for Sales vs Week
plot(tuna$week, tuna$sal1, type="l", col="blue", lwd=2,
     xlab="Week", ylab="Sales of Brand 1 (units)", 
     main="Weekly Sales of Brand 1 Tuna")

# Plot for Price vs Week
plot(tuna$week, tuna$apr1, type="l", col="red", lwd=2,
     xlab="Week", ylab="Price of Brand 1 ($)", 
     main="Weekly Price of Brand 1 Tuna")

# How much variation
# Calculate Coefficient of Variation (CV)
cv_sal1 <- (sd(tuna$sal1) / mean(tuna$sal1)) * 100
cv_apr1 <- (sd(tuna$apr1) / mean(tuna$apr1)) * 100

cat("CV for Sales:", cv_sal1, "%\n")
cat("CV for Price:", cv_apr1, "%\n")

# ==========================================
# PART B: Scatterplot of Sales vs Price
# ==========================================

plot(tuna$apr1, tuna$sal1, pch=16, col="darkgray",
     xlab="Price of Brand 1 ($)", ylab="Sales of Brand 1 (units)", 
     main="Sales vs. Price for Brand 1 Tuna")

# ==========================================
# PART C: Linear Regression of Sales vs Price
# ==========================================

# 1. Create the new variable PRICE1 (price in cents)
tuna$PRICE1 <- 100 * tuna$apr1

# 2. Estimate the linear regression model
model_c <- lm(sal1 ~ PRICE1, data=tuna)

# 3. Print the summary to get the point estimate (the Estimate for PRICE1)
summary(model_c)

# 4. Calculate the 95% confidence interval for the parameters
confint(model_c, level=0.95)

# ==========================================
# PART D: 90% Interval Estimate for Part C
# ==========================================

# 1. Set up the new data point: price is 70 cents
new_price <- data.frame(PRICE1 = 70)

# 2. Calculate the expected value and 90% confidence interval
prediction_d <- predict(model_c, newdata = new_price, interval = "confidence", level = 0.90)

# 3. Print the exact values
print(prediction_d)

# ==========================================
# PART E: 95% Interval Estimate of the Elasticity of Sales
# ==========================================

# 1. Calculate the sample means
mean_sal1 <- mean(tuna$sal1)
mean_price1 <- mean(tuna$PRICE1)

# 2. Extract the point estimate and confidence interval for b2 from model_c
b2 <- coef(model_c)["PRICE1"]
ci_b2 <- confint(model_c, level=0.95)["PRICE1", ]

# 3. Calculate the constant multiplier (Mean Price / Mean Sales)
multiplier <- mean_price1 / mean_sal1

# 4. Calculate the elasticity point estimate and interval
elasticity <- b2 * multiplier
ci_elasticity <- ci_b2 * multiplier

# Print results
cat("Point Estimate of Elasticity:", elasticity, "\n")
cat("95% CI for Elasticity: [", ci_elasticity[1], ",", ci_elasticity[2], "]\n")

# ==========================================
# PART F: Hypothesis Test on the elasticity
# ==========================================

# 1. Define the constant multiplier (mean_ratio)
mean_ratio <- mean_price1 / mean_sal1

# 2. Extract b2 and its standard error from your model
b2 <- coef(model_c)["PRICE1"]
se_b2 <- coef(summary(model_c))["PRICE1", "Std. Error"]

# 3. Calculate estimated elasticity and its standard error
epsilon_hat <- b2 * mean_ratio
se_epsilon <- se_b2 * mean_ratio

# 4. Calculate the t-statistic for H0: elasticity = -3
t_stat_f <- (epsilon_hat - (-3)) / se_epsilon

# 5. Calculate the two-tailed p-value (df = 52 - 2 = 50)
p_value_f <- 2 * pt(abs(t_stat_f), df = 50, lower.tail = FALSE)

# 6. Find the critical value for a 10% two-tailed test (alpha/2 = 0.05)
t_crit_f <- qt(0.95, df = 50)

# Print results
cat("Test Statistic (t):", t_stat_f, "\n")
cat("Critical Value:", t_crit_f, "\n")
cat("p-value:", p_value_f, "\n")
