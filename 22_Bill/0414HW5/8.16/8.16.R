load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW5/Data/vacation.rdata")
df <- vacation
names(df)

# a.
m_a <- lm(miles ~ income + age + kids, data = vacation)
summary(m_a)
confint(m_a, "kids", level = 0.95)

# b.
resid_m_a <- resid(m_a)
plot(vacation$income, resid_m_a,
     xlab = "INCOME",
     ylab = "Residuals",
     main = "Residuals vs INCOME")
abline(h = 0, col = "red")

plot(vacation$age, resid_m_a,
     xlab = "AGE",
     ylab = "Residuals",
     main = "Residuals vs AGE")
abline(h = 0, col = "red")

# c.
vacation_sorted <- vacation[order(vacation$income), ]

low  <- vacation_sorted[1:90, ]
high <- vacation_sorted[111:200, ]

m_low  <- lm(miles ~ income + age + kids, data = low)
m_high <- lm(miles ~ income + age + kids, data = high)

RSS_low  <- sum(resid(m_low)^2)
RSS_high <- sum(resid(m_high)^2)

df <- 90 - 4
F_stat <- (RSS_high / df) / (RSS_low / df)
F_stat

critical_value <- qf(0.95, df, df)
if (F_stat >= critical_value){
  cat("Reject H0")
}else{
  cat("Do not reject H0")
}

# d.
# robust standard errors
coeftest(m_a, vcov = vcovHC(m_a, type = "HC1"))

b4 <- coef(m_a)["kids"]
robust_se <- sqrt(diag(vcovHC(m_a, type = "HC1")))
se_kids <- robust_se["kids"]

t_val <- qt(0.975, df = 200 - 4)
lower <- b4 - t_val * se4_robust
upper <- b4 + t_val * se4_robust
c(lower, upper)

# e.
w <- 1 / (vacation$income^2)

m_gls <- lm(miles ~ income + age + kids, data = vacation, weights = w)
summary(m_gls)

# traditional SE
b4_gls <- coef(m_gls)["kids"]
se4_gls <- summary(m_gls)$coefficients["kids", "Std. Error"]

t_val <- qt(0.975, df = 200 - 4)
lower_gls <- b4_gls - t_val * se4_gls
upper_gls <- b4_gls + t_val * se4_gls
c(lower_gls, upper_gls)

# robust SE
coeftest(m_gls, vcov = vcovHC(m_gls, type = "HC1"))

robust_se_gls <- sqrt(diag(vcovHC(m_gls, type = "HC1")))
se4_gls_robust <- robust_se_gls["kids"]

lower_gls_r <- b4_gls - t_val * se4_gls_robust
upper_gls_r <- b4_gls + t_val * se4_gls_robust
c(lower_gls_r, upper_gls_r)
