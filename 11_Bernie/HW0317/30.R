library(POE5Rdata)
data("tuna")

# 先看資料結構
str(tuna)
head(tuna)
names(tuna)

# 如果資料裡有 week 變數就用它；若沒有就自動建立 1~n 的週次
if ("week" %in% names(tuna)) {
  week_var <- tuna$week
} else {
  week_var <- 1:nrow(tuna)
}

# =========================================================
# (a) Summary statistics for SAL1 and APR1
# =========================================================

mean_sal1 <- mean(tuna$sal1, na.rm = TRUE)
min_sal1  <- min(tuna$sal1, na.rm = TRUE)
max_sal1  <- max(tuna$sal1, na.rm = TRUE)
sd_sal1   <- sd(tuna$sal1, na.rm = TRUE)

mean_apr1 <- mean(tuna$apr1, na.rm = TRUE)
min_apr1  <- min(tuna$apr1, na.rm = TRUE)
max_apr1  <- max(tuna$apr1, na.rm = TRUE)
sd_apr1   <- sd(tuna$apr1, na.rm = TRUE)

cat("========== Part (a) ==========\n")
cat("SAL1:\n")
cat("Mean =", mean_sal1, "\n")
cat("Min  =", min_sal1, "\n")
cat("Max  =", max_sal1, "\n")
cat("SD   =", sd_sal1, "\n\n")

cat("APR1:\n")
cat("Mean =", mean_apr1, "\n")
cat("Min  =", min_apr1, "\n")
cat("Max  =", max_apr1, "\n")
cat("SD   =", sd_apr1, "\n\n")

# 也可整理成表
summary_table <- data.frame(
  Variable = c("SAL1", "APR1"),
  Mean = c(mean_sal1, mean_apr1),
  Min = c(min_sal1, min_apr1),
  Max = c(max_sal1, max_apr1),
  SD = c(sd_sal1, sd_apr1)
)
print(summary_table)

# Plot SAL1 vs WEEK
plot(week_var, tuna$sal1, type = "b", pch = 19,
     xlab = "WEEK", ylab = "SAL1",
     main = "SAL1 versus WEEK")

# Plot APR1 vs WEEK
plot(week_var, tuna$apr1, type = "b", pch = 19,
     xlab = "WEEK", ylab = "APR1",
     main = "APR1 versus WEEK")

# =========================================================
# (b) Plot SAL1 against APR1
# =========================================================

cat("\n========== Part (b) ==========\n")
plot(tuna$apr1, tuna$sal1, pch = 19,
     xlab = "APR1 (price per can, dollars)",
     ylab = "SAL1 (weekly unit sales)",
     main = "SAL1 against APR1")
abline(lm(sal1 ~ apr1, data = tuna), lwd = 2)

cor_sal1_apr1 <- cor(tuna$sal1, tuna$apr1, use = "complete.obs")
cat("Correlation between SAL1 and APR1 =", cor_sal1_apr1, "\n")

# =========================================================
# (c) Create PRICE1 = 100*APR1
# Estimate SAL1 = beta1 + beta2*PRICE1 + e
# Effect of one cent increase in price on sales
# 95% CI for beta2
# =========================================================

tuna$price1 <- 100 * tuna$apr1   # price in cents

mod <- lm(sal1 ~ price1, data = tuna)
mod_sum <- summary(mod)

cat("\n========== Part (c) ==========\n")
print(mod_sum)

b1 <- as.numeric(coef(mod)["(Intercept)"])
b2 <- as.numeric(coef(mod)["price1"])
se_b2 <- as.numeric(coef(mod_sum)["price1", "Std. Error"])
df <- df.residual(mod)
t_crit_95 <- qt(0.975, df)

ci_b2_low  <- b2 - t_crit_95 * se_b2
ci_b2_high <- b2 + t_crit_95 * se_b2

cat("b1_hat =", b1, "\n")
cat("b2_hat =", b2, "\n")
cat("Interpretation of b2_hat: one-cent increase in price changes sales by", b2, "units.\n")
cat("95% CI for b2 = (", ci_b2_low, ",", ci_b2_high, ")\n\n")

# =========================================================
# (d) 90% interval estimate for expected number sold
# when price per can is 70 cents
# Since PRICE1 is in cents, use PRICE1 = 70
# =========================================================

cat("========== Part (d) ==========\n")
pred_70 <- predict(mod,
                   newdata = data.frame(price1 = 70),
                   interval = "confidence",
                   level = 0.90)

print(pred_70)

fit_70 <- pred_70[1, "fit"]
lwr_70 <- pred_70[1, "lwr"]
upr_70 <- pred_70[1, "upr"]

cat("Estimated expected sales at 70 cents =", fit_70, "\n")
cat("90% CI for expected sales = (", lwr_70, ",", upr_70, ")\n\n")

# =========================================================
# (e) 95% CI for elasticity at the means
# Elasticity at means = beta2 * (mean(price1) / mean(sal1))
# Treat sample means as constants
# se(elasticity_hat) = (mean(price1)/mean(sal1)) * se(beta2_hat)
# =========================================================

cat("========== Part (e) ==========\n")
mean_price1 <- mean(tuna$price1, na.rm = TRUE)
mean_sal1   <- mean(tuna$sal1, na.rm = TRUE)

elas_hat <- b2 * (mean_price1 / mean_sal1)
se_elas  <- (mean_price1 / mean_sal1) * se_b2

ci_elas_low  <- elas_hat - t_crit_95 * se_elas
ci_elas_high <- elas_hat + t_crit_95 * se_elas

cat("Mean(price1) =", mean_price1, "\n")
cat("Mean(sal1)   =", mean_sal1, "\n")
cat("Elasticity at means =", elas_hat, "\n")
cat("SE(elasticity) =", se_elas, "\n")
cat("95% CI for elasticity = (", ci_elas_low, ",", ci_elas_high, ")\n\n")

# =========================================================
# (f) Test H0: elasticity = -3
# vs H1: elasticity != -3
# alpha = 0.10
# =========================================================

cat("========== Part (f) ==========\n")
alpha <- 0.10
t_crit_10_two_tail <- qt(1 - alpha/2, df)

t_stat_f <- (elas_hat - (-3)) / se_elas
p_value_f <- 2 * (1 - pt(abs(t_stat_f), df))

cat("H0: elasticity = -3\n")
cat("H1: elasticity != -3\n")
cat("t statistic =", t_stat_f, "\n")
cat("Critical values = +/-", t_crit_10_two_tail, "\n")
cat("p-value =", p_value_f, "\n")

if (abs(t_stat_f) > t_crit_10_two_tail) {
  cat("Decision: Reject H0 at 10% significance level.\n")
} else {
  cat("Decision: Do not reject H0 at 10% significance level.\n")
}

# =========================================================
# Optional: more compact answer summary
# =========================================================

cat("\n========== Compact Summary ==========\n")
cat("(a) Mean SAL1 =", mean_sal1, ", SD SAL1 =", sd_sal1, "\n")
cat("(a) Mean APR1 =", mean_apr1, ", SD APR1 =", sd_apr1, "\n")
cat("(c) b2_hat =", b2, ", 95% CI = (", ci_b2_low, ",", ci_b2_high, ")\n")
cat("(d) E[SAL1|price1=70] =", fit_70, ", 90% CI = (", lwr_70, ",", upr_70, ")\n")
cat("(e) Elasticity at means =", elas_hat, ", 95% CI = (", ci_elas_low, ",", ci_elas_high, ")\n")
cat("(f) t =", t_stat_f, ", p-value =", p_value_f, "\n")