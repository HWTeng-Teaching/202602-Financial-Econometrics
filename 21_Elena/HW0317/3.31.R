setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("tuna.rdata")
# (a)
summary(tuna$sal1)
sd(tuna$sal1)

summary(tuna$apr1)
sd(tuna$apr1)

tuna$week <- 1:52
plot(tuna$week, tuna$sal1, type="l", main="Weekly Sales")
plot(tuna$week, tuna$apr1, type="l", main="Weekly Price")

# (b)
plot(tuna$apr1, tuna$sal1, xlab="Price", ylab="Sales", main="Demand Curve")

# (c)
tuna$price1 <- 100 * tuna$apr1
model_c <- lm(sal1 ~ price1, data=tuna)
summary(model_c)

confint(model_c, "price1", level=0.95) # 95% 置信區間

# (d)
new_p <- data.frame(price1 = 70)
predict(model_c, newdata=new_p, interval="confidence", level=0.90)

#(e)
avg_p1 <- mean(tuna$price1)
avg_sal <- mean(tuna$sal1)
beta2 <- coef(model_c)["price1"]
elasticity <- beta2 * (avg_p1 / avg_sal) # 計算彈性點估計


ci_beta2 <- confint(model_c, "price1", level=0.95) # 計算彈性 95% 置信區間 
ci_elasticity <- ci_beta2 * (avg_p1 / avg_sal) # 區間 = (beta2_lower * (P/Q), beta2_upper * (P/Q)

print(elasticity) # elasticity
print(ci_elasticity) #  95% Confidence Interval

# (f)

alpha_f   <- 0.10
df  <- df.residual(model_c)
se_beta2  <- summary(model_c)$coefficients["price1", "Std. Error"]
h0_value  <- -3
se_elasticity <- se_beta2 * (avg_p1 / avg_sal)

t0_f <- (elasticity - h0_value) / se_elasticity
tc_f <- qt(1 - alpha_f/2, df)
p_val_f <- 2 * pt(abs(t0_f), df, lower.tail = FALSE)

cat("Hypotheses\n")
cat("   H0: elasticity = -3\n")
cat("   H1: elasticity != -3\n\n")

cat("Test Statistic (t0)\n")
cat("   t0 =", t0_f, "\n\n")

cat("Reject Region\n")
cat("   Reject H0 if |t| >", tc_f, "\n\n")

cat("P-value\n")
cat("   p-value =", p_val_f, "\n\n")

cat("Conclusion\n")
if (p_val_f < alpha_f) {
  cat("   Conclusion: p-value < 0.10, Reject H0.\n")
} else {
  cat("   Conclusion: p-value > 0.10, Don't reject H0.\n")
}