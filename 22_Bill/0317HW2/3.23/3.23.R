load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW2/Data/collegetown.rdata")
df <- collegetown
names(df)

# a.
m_a <- lm(price ~ I(sqft ^ 2), data = df)    # I(): construct a new variable, sqft^2
summary(m_a)
alpha2_hat <- coef(m_a)["I(sqft^2)"]
SE_alpha2 <- summary(m_a)$coefficients["I(sqft^2)", "Std. Error"]
t_stat_a <- (alpha2_hat - 13 / 40) / SE_alpha2    # test statistic
d_f <- df.residual(m_a)    # degree of freedom
crit <- qt(0.95, df = d_f)    # critical value
# rejection region = P(t ≥ crit)
pt(t_stat_a, df = d_f)    # P(t < t0 | H0 True)
p_value_a <- 1 - pt(t_stat_a, df = d_f)    # P(t ≥ t0 | H0 True)

cat("p_value = ", p_value_a)
cat("rejection region = ", crit)
if (p_value_a <= 0.05){
  cat("Reject H0")
} else{
  cat("Do not reject H0")
}

# b.
t_stat_b <- (alpha2_hat - 13 / 80) / SE_alpha2
t_stat_b
p_value_b <- 1 - pt(t_stat_b, df = d_f)
p_value_b
cat("p_value = ", p_value_b)
cat("rejection region = ", crit)
if (p_value_b <= 0.05){
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
mean_2000 <- mean(df_2000$price)
mean_2000
