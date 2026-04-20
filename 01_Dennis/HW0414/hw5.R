# Homework 5 - Chapter 8
# Question 8.6

# a.
# H0: var_M = var_F vs HA: var_M != var_F
SSE_M <- 97161.9174
N_M <- 577
K <- 4 # WAGE = b1 + b2 EDUC + b3 EXPER + b4 METRO + e
df_M <- N_M - K
var_M <- SSE_M / df_M

sigma_F_hat <- 12.024
var_F <- sigma_F_hat^2
N_F <- 1000 - 577
df_F <- N_F - K

F_stat_a <- var_M / var_F
# F_crit
F_crit_a_lower <- qf(0.025, df_M, df_F)
F_crit_a_upper <- qf(0.975, df_M, df_F)
p_val_a <- 2 * min(pf(F_stat_a, df_M, df_F), 1 - pf(F_stat_a, df_M, df_F))

cat("8.6a\n")
cat("var_M: ", var_M, "\nvar_F: ", var_F, "\nF-stat: ", F_stat_a, "\n")
cat("Rejection region: F <", F_crit_a_lower, "or F >", F_crit_a_upper, "\n\n")

# b.
# H0: var_S = var_M vs HA: var_M > var_S
SSE_S <- 56231.0382
N_S <- 400
K_b <- 5 # WAGE = b1 + b2 EDUC + b3 EXPER + b4 METRO + b5 FEMALE + e
df_S <- N_S - K_b
var_S <- SSE_S / df_S

SSE_M_b <- 100703.0471
N_M_b <- 600
df_M_b <- N_M_b - K_b
var_M_b <- SSE_M_b / df_M_b

F_stat_b <- var_M_b / var_S
F_crit_b <- qf(0.95, df_M_b, df_S)
p_val_b <- 1 - pf(F_stat_b, df_M_b, df_S)

cat("8.6b\n")
cat("var_S: ", var_S, "\nvar_M: ", var_M_b, "\nF-stat: ", F_stat_b, "\n")
cat("Rejection region: F >", F_crit_b, "\n\n")

# c.
LM_c <- 59.03
df_c <- 4 # EDUC, EXPER, METRO, FEMALE
chi_crit_c <- qchisq(0.95, df_c)
p_val_c <- 1 - pchisq(LM_c, df_c)

cat("8.6c\n")
cat("LM: ", LM_c, "\nCritical value: ", chi_crit_c, "\n\n")

# d.
LM_d <- 78.82
# Variables: EDUC, EXPER, METRO, FEMALE
# Number of regressors (S) in White test = 4 + 2 + 6 = 12
df_d <- 12
chi_crit_d <- qchisq(0.95, df_d)
p_val_d <- 1 - pchisq(LM_d, df_d)

cat("8.6d\n")
cat("df: ", df_d, "\nCritical value: ", chi_crit_d, "\n\n")


# ---------------------------------------------------------
# Question 8.16

library(POE5Rdata)
library(sandwich)
library(lmtest)
library(stargazer)

data("vacation")

# a.
mod_a <- lm(miles ~ income + age + kids, data = vacation)
b4 <- coef(mod_a)["kids"]
se4 <- sqrt(diag(vcov(mod_a)))["kids"]
t_crit <- qt(0.975, mod_a$df.residual)

ci_lower_a <- b4 - t_crit * se4
ci_upper_a <- b4 + t_crit * se4

cat("8.16a\n")
cat("95% CI for kids: [", ci_lower_a, ", ", ci_upper_a, "]\n\n")

# c.
vacation_sorted <- vacation[order(vacation$income),]
mod_c_lower <- lm(miles ~ income + age + kids, data = vacation_sorted[1:90, ])
mod_c_upper <- lm(miles ~ income + age + kids, data = vacation_sorted[111:200, ])

var_lower <- sum(residuals(mod_c_lower)^2) / mod_c_lower$df.residual
var_upper <- sum(residuals(mod_c_upper)^2) / mod_c_upper$df.residual

F_stat_c <- var_upper / var_lower
df_lower <- mod_c_lower$df.residual
df_upper <- mod_c_upper$df.residual
F_crit_c <- qf(0.95, df_upper, df_lower)
p_val_c <- 1 - pf(F_stat_c, df_upper, df_lower)

cat("8.16c\n")
cat("GQ F-stat: ", F_stat_c, "\nCritical value: ", F_crit_c, "\np-value: ", p_val_c, "\n\n")

# d.
cov_hc1 <- vcovHC(mod_a, type = "HC1")
se_hc1 <- sqrt(diag(cov_hc1))["kids"]
ci_lower_d <- b4 - t_crit * se_hc1
ci_upper_d <- b4 + t_crit * se_hc1

cat("8.16d\n")
cat("Robust 95% CI for kids: [", ci_lower_d, ", ", ci_upper_d, "]\n\n")

# e.
mod_e <- lm(miles ~ income + age + kids, data = vacation, weights = 1/income^2)
b4_e <- coef(mod_e)["kids"]
se4_e <- sqrt(diag(vcov(mod_e)))["kids"]
t_crit_e <- qt(0.975, mod_e$df.residual)

ci_lower_e_conv <- b4_e - t_crit_e * se4_e
ci_upper_e_conv <- b4_e + t_crit_e * se4_e

cov_hc1_e <- vcovHC(mod_e, type = "HC1")
se_hc1_e <- sqrt(diag(cov_hc1_e))["kids"]
ci_lower_e_rob <- b4_e - t_crit_e * se_hc1_e
ci_upper_e_rob <- b4_e + t_crit_e * se_hc1_e

cat("8.16e\n")
cat("GLS Conv 95% CI: [", ci_lower_e_conv, ", ", ci_upper_e_conv, "]\n")
cat("GLS Robust 95% CI: [", ci_lower_e_rob, ", ", ci_upper_e_rob, "]\n")
