load("Documents/R/data_needed/poe5rdata/collegetown.rdata")

# PRICE = a1 + a2 x SQFT^2 + e
# marginal effect = 2 * a2 * SQFT 

# hypothesis test (right-tailed)
# H0: 40*a2 <= 13 v.s. H1: 40*a2 > 13
alpha <- 0.05
model1 <- lm(price ~ I(sqft^2), data = collegetown)
summary(model1)

# extract coefficient and degree of freedom
a2 = coef(model1)[["I(sqft^2)"]]
df <- df.residual(model1)

# --------------------------------------------------------
# part a

sqrt_a <- 20
me_a <- 2 * 20 * a2
se_a2 <- summary(model1)$coefficient["I(sqft^2)", "Std. Error"]
se_a <- 2 * 20 * se_a2
t_star_a <- (me_a - 13) / se_a

# rejection region
# if t* > tc, reject H0
tc <- qt(1-alpha, df)

# p-value = P(T > t*)
p_value_a <- pt(t_star_a, df, lower.tail = FALSE)


# conclusion
if(p_value_a < alpha){
  cat("reject H0")
} else {
  cat("Do not reject H0")
}

# --------------------------------------------------------
# part b

sqrt_b <- 40
me_b <- 2 * 40 * a2
se_a2 <- summary(model1)$coefficient["I(sqft^2)", "Std. Error"]
se_b <- 2 * 40 * se_a2
t_star_b <- (me_b - 13) / se_b

# rejection region
# if t* > tc, reject H0
tc <- qt(1-alpha, df)

# p-value = P(T > t*)
p_value_b <- pt(t_star_b, df, lower.tail = FALSE)


# conclusion
if(p_value_b < alpha){
  cat("reject H0")
} else {
  cat("Do not reject H0")
}

# --------------------------------------------------------
# part c
# E(Price|SQFT) = a1 + a2 * SQFT^2
a1 <- coef(model1)["(Intercept)"]
price_c <- a1 + a2 * 20^2
cat(price_c)

# CI
ci_c <- predict(model1,
                newdata = data.frame(sqft = 20),
                interval = "confidence",
                level = 0.95)
cat("95% confidence interval:[", ci_c[, "lwr"], ",", ci_c[, "upr"], "]")

# --------------------------------------------------------
# part d

house_20 <- subset(collegetown, sqft == 20)
sample_mean <- mean(house_20$price)
cat("The sample average prices of these houses is ", sample_mean, ".")

if(sample_mean > ci_c[, "lwr"] && sample_mean < ci_c[, "upr"]){
  cat("The sample average falls within the interval. Therefore, the result is compatible with the result in c.")
}else{
  cat("The sample average doesn't fall within the interval. Therefore, the result is incompatible with the result in c.")
}