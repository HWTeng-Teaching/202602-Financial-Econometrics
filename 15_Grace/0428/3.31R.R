
load("Documents/R/data_needed/poe5rdata/tuna.rdata")

# part a
summary(tuna$sal1)
summary(tuna$apr1)
sd(tuna$sal1)
sd(tuna$apr1)

library(ggplot2)
tuna$WEEK <- 1:52
ggplot(data = tuna, mapping = aes(x = WEEK, y = sal1)) +
  geom_point(color ="steelblue", alpha = 0.5, size = 1.5)

ggplot(data = tuna, mapping = aes(x = WEEK, y = apr1)) +
  geom_point(color ="lightpink", alpha = 0.5, size = 1.5)

#------------------------------------------------------------
# part b
ggplot(data = tuna, mapping = aes(x = apr1, y = sal1)) +
  geom_point(color ="brown", alpha = 0.5, size = 1.5)

#------------------------------------------------------------
# part c
tuna$price1 <- 100 * tuna$apr1
#linear regression: SAL1 = B1 + B2 x PRICE1 + e
model2 <- lm(sal1~price1, data = tuna)
b2 <- coef(model2)["price1"]

# CI
ci_b2 <- confint(model2, level = 0.95)
print(ci_b2)
lowb_b2 <- ci_b2[2,1]
upb_b2 <- ci_b2[2,2]

#------------------------------------------------------------
# part d
# linear regression: SAL1 = B1 + B2 x PRICE1 + e
# price = 70 cents

# CI
ci_d <- predict(model2,
                newdata = data.frame(price1 = 70),
                interval = "confidence",
                level = 0.9)
cat("90% confidence interval:[", ci_d[, "lwr"], ",", ci_d[, "upr"], "]")

#------------------------------------------------------------
# part e
# elasticity = B2 x price1_mean / sal1_mean

# CI
e <- b2 * mean(tuna$price1) / mean(tuna$sal1)
lowb_elasticity <- ci_b2[2,1] * mean(tuna$price1) / mean(tuna$sal1)
upb_elasticity <- ci_b2[2,2] * mean(tuna$price1) / mean(tuna$sal1)
cat("95% confidence interval of elasticity of sales with respect to the price:[", lowb_elasticity, ",", upb_elasticity, "]")

#------------------------------------------------------------
# part f
# hypothesis test
# H0: elasticity = -3 vs. H1: elasticity != -3
df <- df.residual(model2)
alpha2 <- 0.1

# standard error of b2 = 78.58
model2_summary <- summary(model2)
coef_table <- model2_summary$coefficients
se_b2 <- coef_table["price1", "Std. Error"]
se_e <- se_b2 * mean(tuna$price1) / mean(tuna$sal1) 
t_star_e <- (e - (-3)) / se_e

# rejection region
# if t* > tc, reject H0
tc <- qt(1-alpha2/2, df)

# p-value = P(T > t*)
p_value_e <- 2 * pt(abs(t_star_e), df, lower.tail = FALSE)


# conclusion
if(p_value_e < alpha2){
  cat("reject H0")
} else {
  cat("Do not reject H0")
}
