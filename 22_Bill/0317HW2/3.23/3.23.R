load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW2/Data/collegetown.rdata")
df <- collegetown
names(df)

# a.
m_a <- lm(price ~ I(sqft ^ 2), data = df)    # I(): construct a new variable, sqft^2
summary(m_a)
alpha2_hat <- coef(m_a)["I(sqft^2)"]
SE_alpha2 <- summary(m_a)$coefficients["I(sqft^2)", "Std. Error"]
t_stat_a <- (alpha2_hat - 13 / 40) / SE_alpha2
d_f <- df.residual(m_a)
crit <- qt(0.95, df = d_f)
crit
p_value_a <- 1 - pt(t_stat_a, df = d_f)

cat("p_value = ", p_value_a)
cat("rejection region = ", crit)
if (p_value_a <= 0.05){
  cat("Reject H0")
} else{
  cat("Do not reject H0")
}

# b.
# Hypothesis Testing
# Increasing the size of a 4000 square foot house 
# H0: by 100 square feet ≤ $13,000 increase of expected house price
# Ha: by 100 square feet > $13,000 increase of expected house price
ME_hat_b <- 2 * alpha2_hat * 40
SE_ME_b <- 2 * SE_alpha2 * 40
t_stat_b <- (ME_hat_b - 13) / SE_ME_b
p_value_b <- 1 - pt(t_stat_b, df = d_f)

cat("p_value = ", p_value_b)
cat("rejection region = ", crit)
if (t_stat_a > crit){
  cat("Reject H0")
} else{
  cat("Do not reject H0")
}
if (t_stat_b > crit){
  cat("Reject H0")
} else{
  cat("Do not reject H0")
}

# c.
expected_price <- data.frame(sqft = 20)
predict(m_a, newdata = expected_price)
predict(m_a, newdata = expected_price, interval = "confidence", level = 0.95)

# d.
df_2000 <- subset(df, sqft == 20)
n_2000 <- nrow(df_2000)
n_2000
mean_2000 <- mean(df_2000$price)
mean_2000
