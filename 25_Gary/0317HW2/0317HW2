# Part 1: Exercise 3.31 - Canned Tuna Sales Analysis

# 1.1 Data Preparation
load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW2/Data/tuna.rdata")
df_tuna <- tuna

# 1.2 Summary Statistics (Part a)
print("--- Summary Statistics for SAL1 and APR1 ---")
summary(df_tuna$sal1)
cat("SD(SAL1):", sd(df_tuna$sal1), "\n")
summary(df_tuna$apr1)
cat("SD(APR1):", sd(df_tuna$apr1), "\n")

# 1.3 Time Series Visualization (Part a)
par(mfrow = c(2, 1))
plot(1:52, df_tuna$sal1, type = "b", pch = 16, col = "darkblue",
     xlab = "WEEK", ylab = "SAL1", main = "Weekly Sales Variation")
plot(1:52, df_tuna$apr1, type = "b", pch = 16, col = "darkred",
     xlab = "WEEK", ylab = "APR1", main = "Weekly Price Variation")
par(mfrow = c(1, 1))

# 1.4 Scatter Plot (Part b)
plot(df_tuna$apr1, df_tuna$sal1, pch = 16, col = "steelblue",
     xlab = "Price (APR1)", ylab = "Sales (SAL1)",
     main = "Inverse Relationship: Sales vs Price")
abline(lm(sal1 ~ apr1, data = df_tuna), col = "red", lwd = 2)

# 1.5 Linear Regression Analysis (Part c)
df_tuna$price1 <- 100 * df_tuna$apr1
m_tuna <- lm(sal1 ~ price1, data = df_tuna)
summary(m_tuna)
confint(m_tuna, "price1", level = 0.95)

# 1.6 Interval Estimate for Predicted Sales (Part d)
# Expected sales when price is 70 cents
pred_sal_70 <- predict(m_tuna, newdata = data.frame(price1 = 70), 
                       interval = "confidence", level = 0.90)
print(pred_sal_70)

# 1.7 Price Elasticity at the Means (Part e)
k <- mean(df_tuna$price1) / mean(df_tuna$sal1)
elas_point <- coef(m_tuna)["price1"] * k
elas_ci_95 <- confint(m_tuna, "price1", level = 0.95) * k
cat("Point Elasticity:", elas_point, "\n")
cat("95% Elasticity CI:", elas_ci_95, "\n")

# 1.8 Hypothesis Test for Elasticity = -3 (Part f)
se_elas <- summary(m_tuna)$coefficients["price1", "Std. Error"] * abs(k)
t_stat_f <- (elas_point - (-3)) / se_elas
p_val_f <- 2 * pt(-abs(t_stat_f), df = df.residual(m_tuna))

cat("Elasticity Test (H0: E = -3): t =", t_stat_f, "p-value =", p_val_f, "\n")
if (p_val_f <= 0.1) cat("Decision: Reject H0\n") else cat("Decision: Do not reject H0\n")

# Part 2: Exercise 3.23 - Collegetown Housing Price Analysis

# 2.1 Data Preparation
load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW2/Data/collegetown.rdata")
df_house <- collegetown

# 2.2 Quadratic Model Construction (Part a)
m_house <- lm(price ~ I(sqft^2), data = df_house)
summary(m_house)

alpha2_hat <- coef(m_house)["I(sqft^2)"]
se_alpha2 <- summary(m_house)$coefficients["I(sqft^2)", "Std. Error"]

# 2.3 Marginal Effect Test: SQFT = 20 (Part a)
# H0: 40*alpha2 <= 13 vs H1: 40*alpha2 > 13
t_stat_a <- (alpha2_hat - 13/40) / se_alpha2
p_val_a <- 1 - pt(t_stat_a, df = df.residual(m_house))
cat("Test Part A (2000 sqft): t =", t_stat_a, "p-value =", p_val_a, "\n")

# 2.4 Marginal Effect Test: SQFT = 40 (Part b)
# H0: 80*alpha2 <= 13 vs H1: 80*alpha2 > 13
t_stat_b <- (alpha2_hat - 13/80) / se_alpha2
p_val_b <- 1 - pt(t_stat_b, df = df.residual(m_house))
cat("Test Part B (4000 sqft): t =", t_stat_b, "p-value =", p_val_b, "\n")

# 2.5 Confidence Interval for Expected Price (Part c)
# Prediction for a 2000 sqft house
predict(m_house, newdata = data.frame(sqft = 20), 
        interval = "confidence", level = 0.95)

# 2.6 Model Compatibility Check (Part d)
mean_observed_20 <- mean(subset(df_house, sqft == 20)$price)
cat("Observed Sample Mean at SQFT=20:", mean_observed_20, "\n")

# Part 3: Model Diagnostics (Optional but Professional)

par(mfrow = c(2, 2))
plot(m_tuna)
par(mfrow = c(1, 1))
