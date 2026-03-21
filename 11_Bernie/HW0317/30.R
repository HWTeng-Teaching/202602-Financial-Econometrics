library(POE5Rdata)
data("star5_small")

# 看變數名稱
names(star5_small)
str(star5_small)

#--------------------------------------------------
# (a) Sample mean and standard deviation
# Regular classes without aide: regular == 1
# Regular classes with aide: aide == 1
#--------------------------------------------------

reg <- subset(star5_small, regular == 1)
aid <- subset(star5_small, aide == 1)

mean_reg <- mean(reg$mathscore, na.rm = TRUE)
sd_reg   <- sd(reg$mathscore, na.rm = TRUE)

mean_aid <- mean(aid$mathscore, na.rm = TRUE)
sd_aid   <- sd(aid$mathscore, na.rm = TRUE)

diff_mean <- mean_aid - mean_reg

cat("Part (a)\n")
cat("Regular class (no aide): mean =", round(mean_reg, 4),
    ", sd =", round(sd_reg, 4), "\n")
cat("Regular class with aide: mean =", round(mean_aid, 4),
    ", sd =", round(sd_aid, 4), "\n")
cat("Difference in sample means (aide - regular) =", round(diff_mean, 4), "\n\n")

#--------------------------------------------------
# (b) Estimate: MATHSCORE = beta1 + beta2*AIDE + e
# Use only regular classes without aide and with aide
#--------------------------------------------------

dat <- subset(star5_small, regular == 1 | aide == 1)

mod <- lm(mathscore ~ aide, data = dat)
summary(mod)

cat("Part (b)\n")
cat("beta1_hat =", round(coef(mod)[1], 4), "\n")
cat("beta2_hat =", round(coef(mod)[2], 4), "\n")
cat("Interpretation:\n")
cat("beta1_hat = sample mean of mathscore for regular classes without aide\n")
cat("beta2_hat = difference in sample means (aide - regular)\n\n")

#--------------------------------------------------
# (c) 95% CI for expected MATHSCORE in each type of class
# regular no aide: E[Y|aide=0] = beta1
# regular with aide: E[Y|aide=1] = beta1 + beta2
#--------------------------------------------------

pred_reg <- predict(mod,
                    newdata = data.frame(aide = 0),
                    interval = "confidence",
                    level = 0.95)

pred_aid <- predict(mod,
                    newdata = data.frame(aide = 1),
                    interval = "confidence",
                    level = 0.95)

width_reg <- pred_reg[1, "upr"] - pred_reg[1, "lwr"]
width_aid <- pred_aid[1, "upr"] - pred_aid[1, "lwr"]

overlap <- !(pred_reg[1, "upr"] < pred_aid[1, "lwr"] ||
               pred_aid[1, "upr"] < pred_reg[1, "lwr"])

cat("Part (c)\n")
cat("95% CI for expected MATHSCORE (regular no aide):\n")
print(pred_reg)

cat("\n95% CI for expected MATHSCORE (regular with aide):\n")
print(pred_aid)

cat("\nCI width for regular no aide =", round(width_reg, 4), "\n")
cat("CI width for regular with aide =", round(width_aid, 4), "\n")
cat("Do the intervals overlap? ", overlap, "\n\n")

#--------------------------------------------------
# (d) Test H0: no difference vs H1: aide class is higher
# H0: beta2 = 0
# H1: beta2 > 0
# alpha = 0.05
#--------------------------------------------------

b2 <- as.numeric(coef(mod)["aide"])
se_b2 <- as.numeric(coef(summary(mod))["aide", "Std. Error"])
df <- df.residual(mod)

t_stat_d <- (b2 - 0) / se_b2
t_crit_d <- qt(0.95, df)        # right-tail test, alpha = 0.05
p_value_d <- 1 - pt(t_stat_d, df)
reject_d <- t_stat_d > t_crit_d

cat("Part (d)\n")
cat("H0: beta2 = 0\n")
cat("H1: beta2 > 0\n")
cat("t statistic =", round(t_stat_d, 4), "\n")
cat("critical value =", round(t_crit_d, 4), "\n")
cat("p-value =", round(p_value_d, 6), "\n")
cat("Reject H0? ", reject_d, "\n\n")

#--------------------------------------------------
# (e) Test H0: difference is three points or more
# vs H1: difference is less than three points
#
# H0: beta2 >= 3
# H1: beta2 < 3
# alpha = 0.10
#--------------------------------------------------

t_stat_e <- (b2 - 3) / se_b2
t_crit_e <- qt(0.10, df)        # left-tail test, alpha = 0.10
p_value_e <- pt(t_stat_e, df)
reject_e <- t_stat_e < t_crit_e

cat("Part (e)\n")
cat("H0: beta2 >= 3\n")
cat("H1: beta2 < 3\n")
cat("t statistic =", round(t_stat_e, 4), "\n")
cat("critical value =", round(t_crit_e, 4), "\n")
cat("p-value =", round(p_value_e, 6), "\n")
cat("Reject H0? ", reject_e, "\n")