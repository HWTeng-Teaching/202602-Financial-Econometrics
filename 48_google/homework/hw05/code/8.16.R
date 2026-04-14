# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 8, Exercise 8.16
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)
library(sandwich)
library(lmtest)
library(ggplot2)

data("vacation")
head(vacation)
summary(vacation)

# ─── (a) OLS estimation ───────────────────────────────────
ols_fit <- lm(miles ~ income + age + kids, data = vacation)
summary(ols_fit)

# 95% CI for beta_4 (KIDS)
ci_ols <- confint(ols_fit, level = 0.95)
cat("\n=== (a) OLS 95% CI for KIDS ===\n")
print(ci_ols["kids", ])

# ─── (b) Residual plots ───────────────────────────────────
vacation$resid_ols <- residuals(ols_fit)

# Plot residuals vs INCOME
p_income <- ggplot(vacation, aes(x = income, y = resid_ols)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "OLS Residuals vs INCOME",
       x = "INCOME (thousand $)", y = "Residuals") +
  theme_bw()


# Plot residuals vs AGE
p_age <- ggplot(vacation, aes(x = age, y = resid_ols)) +
  geom_point(alpha = 0.7, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "OLS Residuals vs AGE",
       x = "AGE (years)", y = "Residuals") +
  theme_bw()


cat("\n=== (b) Residual plots saved ===\n")

# ─── (c) Goldfeld-Quandt test ─────────────────────────────
# Sort by INCOME
vacation_sorted <- vacation[order(vacation$income), ]

# First 90 observations (low income)
data_low <- vacation_sorted[1:90, ]
fit_low  <- lm(miles ~ income + age + kids, data = data_low)
sse_low  <- sum(residuals(fit_low)^2)
df_low   <- 90 - 4

# Last 90 observations (high income)
data_high <- vacation_sorted[111:200, ]
fit_high  <- lm(miles ~ income + age + kids, data = data_high)
sse_high  <- sum(residuals(fit_high)^2)
df_high   <- 90 - 4

cat("\n=== (c) Goldfeld-Quandt Test ===\n")
cat(sprintf("SSE_low  = %.4f, df = %d\n", sse_low, df_low))
cat(sprintf("SSE_high = %.4f, df = %d\n", sse_high, df_high))

# F statistic: H1: sigma_high^2 > sigma_low^2
gq_f <- (sse_high / df_high) / (sse_low / df_low)
cat(sprintf("GQ F-statistic = %.4f\n", gq_f))

# Critical value at 5% (one-tailed)
f_crit <- qf(0.95, df_high, df_low)
cat(sprintf("F critical (0.95, %d, %d) = %.4f\n", df_high, df_low, f_crit))
cat(sprintf("p-value = %.6f\n", pf(gq_f, df_high, df_low, lower.tail = FALSE)))
cat(sprintf("Reject H0? %s\n", ifelse(gq_f > f_crit, "YES", "NO")))

# ─── (d) OLS with Robust Standard Errors ─────────────────
cat("\n=== (d) OLS with Robust SE ===\n")
robust_se <- coeftest(ols_fit, vcov = vcovHC(ols_fit, type = "HC1"))
print(robust_se)

# 95% CI for KIDS using robust SE
beta4     <- coef(ols_fit)["kids"]
se_robust <- sqrt(vcovHC(ols_fit, type = "HC1")["kids", "kids"])
t_crit    <- qt(0.975, df = ols_fit$df.residual)
ci_robust_lo <- beta4 - t_crit * se_robust
ci_robust_hi <- beta4 + t_crit * se_robust
cat(sprintf("Robust 95%% CI for KIDS: [%.4f, %.4f]\n", ci_robust_lo, ci_robust_hi))

# ─── (e) GLS assuming sigma_i^2 = sigma^2 * INCOME_i^2 ──
cat("\n=== (e) GLS (divide by INCOME) ===\n")
# Transformed variables: divide everything by INCOME_i
vacation$w         <- 1 / vacation$income
vacation$miles_t   <- vacation$miles   / vacation$income
vacation$income_t  <- vacation$income  / vacation$income   # = 1
vacation$age_t     <- vacation$age     / vacation$income
vacation$kids_t    <- vacation$kids    / vacation$income

# GLS = WLS with weight 1/INCOME^2, or equivalently OLS on transformed data
gls_fit <- lm(miles_t ~ 0 + w + income_t + age_t + kids_t, data = vacation)
summary(gls_fit)

# Conventional GLS SE
ci_gls_conv <- confint(gls_fit, level = 0.95)
cat("\nConventional GLS 95% CI for KIDS (kids_t coefficient):\n")
print(ci_gls_conv["kids_t", ])

# Robust GLS SE
beta4_gls    <- coef(gls_fit)["kids_t"]
se_gls_rob   <- sqrt(vcovHC(gls_fit, type = "HC1")["kids_t", "kids_t"])
t_crit_gls   <- qt(0.975, df = gls_fit$df.residual)
ci_gls_rob_lo <- beta4_gls - t_crit_gls * se_gls_rob
ci_gls_rob_hi <- beta4_gls + t_crit_gls * se_gls_rob
cat(sprintf("Robust GLS 95%% CI for KIDS: [%.4f, %.4f]\n", ci_gls_rob_lo, ci_gls_rob_hi))

cat("\n=== Summary of 95% CIs for KIDS ===\n")
cat(sprintf("(a) OLS conventional  : [%.4f, %.4f]\n",
            ci_ols["kids", 1], ci_ols["kids", 2]))
cat(sprintf("(d) OLS robust        : [%.4f, %.4f]\n", ci_robust_lo, ci_robust_hi))
cat(sprintf("(e) GLS conventional  : [%.4f, %.4f]\n",
            ci_gls_conv["kids_t", 1], ci_gls_conv["kids_t", 2]))
cat(sprintf("(e) GLS robust        : [%.4f, %.4f]\n", ci_gls_rob_lo, ci_gls_rob_hi))


# plot and save in work directory
ggsave("b_residuals_vs_income.png", plot = p_income, dpi = 150, width = 6, height = 4)
ggsave("b_residuals_vs_age.png", plot = p_age, dpi = 150, width = 6, height = 4)
