install.packages("devtools")  # if not already installed
library(devtools)
install_git("https://github.com/ccolonescu/PoEdata")

library(PoEdata) 
data(tuna)

# (a) Summary Statistics
summary_stats <- data.frame(
  Variable = c("SAL1", "APR1"),
  Mean     = c(mean(tuna$sal1), mean(tuna$apr1)),
  Min      = c(min(tuna$sal1),  min(tuna$apr1)),
  Max      = c(max(tuna$sal1),  max(tuna$apr1)),
  SD       = c(sd(tuna$sal1),   sd(tuna$apr1))
)
summary_stats

tuna$week <- 1:nrow(tuna)

plot(tuna$week, tuna$sal1, type="l", col="blue", main="Weekly Sales (SAL1)", xlab="Week", ylab="number of cans of brand no.1 sold")
plot(tuna$week, tuna$apr1, type="l", col="red", main="Weekly Price (APR1)", xlab="Week", ylab="price per can of brand no.1 ($)")

# b. Scatter Plot: SAL1 vs APR1

plot(tuna$apr1, tuna$sal1, main="Sales vs Price(Weekly)", xlab="price per can of brand no.1 ($)", ylab="number of cans of brand no.1 sold ", pch=16, col="darkgreen")

# c. Regression & 95% Confidence Interval
tuna$price1 <- 100 * tuna$apr1
mod <- lm(sal1 ~ price1, data=tuna)

b2 <- coef(mod)["price1"]
se_b2 <- summary(mod)$coef["price1", "Std. Error"]
df_res <- df.residual(mod)
t95 <- qt(0.975, df_res)

ci_c_lower <- b2 - t95 * se_b2
ci_c_upper <- b2 + t95 * se_b2

cat(sprintf("Point Estimate for beta2: %.4f\n", b2))
cat(sprintf("95%% Interval Estimate for beta2: [%.4f, %.4f]\n", ci_c_lower, ci_c_upper))


# d. 90% Expected Value Interval at PRICE1 = 70
newd <- data.frame(price1 = 70)
ci_d <- predict(mod, newdata=newd, interval="confidence", level=0.90)
cat(sprintf("90%% Expected Sales Interval: [%.2f, %.2f]\n", ci_d[,"lwr"], ci_d[,"upr"]))


# e. Elasticity at the Means
mean_sal1 <- mean(tuna$sal1)
mean_price1 <- mean(tuna$price1)

elasticity <- b2 * (mean_price1 / mean_sal1)
se_elasticity <- se_b2 * (mean_price1 / mean_sal1)
ci_e_lower <- elasticity - t95 * se_elasticity
ci_e_upper <- elasticity + t95 * se_elasticity

cat(sprintf("Point Estimate of Elasticity: %.4f\n", elasticity))
cat(sprintf("95%% Interval for Elasticity: [%.4f, %.4f]\n", ci_e_lower, ci_e_upper))


# f. Hypothesis Test for Elasticity = -3
# H0: b2 * (mean_price1 / mean_sal1) = -3
# H0: b2 = -3 * (mean_sal1 / mean_price1)
h0_b2 <- -3 * (mean_sal1 / mean_price1)

t_stat <- (b2 - h0_b2) / se_b2
t90_two <- qt(0.95, df_res) # 10%  two-tailed 
p_val <- 2 * (1 - pt(abs(t_stat), df_res))

cat(sprintf("Test Statistic (t): %.4f\n", t_stat))
cat(sprintf("Rejection Region: |t| > %.4f\n", t90_two))
cat(sprintf("P-value: %.4e\n", p_val))

if(p_val < 0.10) {
  cat("Reject H0.\n")
} else {
  cat("Do not reject H0.\n")
}