load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW2/Data/tuna.rdata")
df <- tuna
names(df)

# a.
summary(df$sal1)
sd(df$sal1)
summary(df$apr1)
sd(df$apr1)
plot(1:52, df$sal1, type = "b",
     xlab = "WEEK", ylab = "SAL1",
     main = "SAL1 versus WEEK")
plot(1:52, df$apr1, type = "b",
     xlab = "WEEK", ylab = "APR1",
     main = "APR1 versus WEEK")

# b.
plot(x = df$apr1,
     y = df$sal1,
     xlab = "APR1",
     ylab = "SAL1",
     main = "SAL1 against APR1")

# c.
df$price1 = I(100 * df$apr1)
m_d = lm(sal1 ~ price1, data = df)
summary(m_d)
coef(m_d)["price1"]
confint(m_d, "price1", data = df, level = 0.95)

# d.
predict(m_d,
        newdata = data.frame(price1 = 70),
        interval = "confidence",
        level = 0.90)

# e.
k <- mean(df$price1) / mean(df$sal1)
confint(m_d, "price1", data = df, level = 0.95) * k

# f.
t_stat_f <- (coef(m_d)["price1"] * k + 3) / (summary(m_d)$coefficients["price1", "Std. Error"] * abs(k))
d_f <- df.residual(m_d)
p_value_f <- 2 * pt(t_stat_f, df = d_f)
if (p_value_f <= 0.1){
  cat("Reject H0")
}else{
  cat("Do not reject H0")
}

# f. (easy)
C_I_90 <- confint(m_d, "price1", data = df, level = 0.90) * k
if (C_I_90[1] <= -3 & -3 <= C_I_90[2]){
  cat("Do not reject H0")
} else{
  cat("reject H0")
}
