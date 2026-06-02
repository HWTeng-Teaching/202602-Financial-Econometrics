# Homework 8 - Chapter 15 Panel Data Models
# Exercises 15.6 and 15.20

library(POE5Rdata)
library(plm)
library(lmtest)
library(car)
library(dplyr)

# =========================================================
# Exercise 15.6
# =========================================================
cat("--- Exercise 15.6 ---\n")

# d. F-test degrees of freedom and critical value
# N = 716, T = 2, NT = 1432. K_s = 4 (EXPER, EXPER2, SOUTH, UNION)
N <- 716
T_obs <- 2
Ks <- 4
df1 <- N - 1
df2 <- N*T_obs - N - Ks

F_stat_156 <- 11.68
F_crit_156 <- qf(0.99, df1, df2)

cat("15.6d: F-test for no individual differences\n")
cat("df1: ", df1, "\ndf2: ", df2, "\n")
cat("1% Critical Value: ", F_crit_156, "\n\n")

# f. Hausman t-test for specific coefficients
# t = (b_FE - b_RE) / sqrt(se_FE^2 - se_RE^2)
calc_hausman_t <- function(b_fe, b_re, se_fe, se_re) {
  var_diff <- se_fe^2 - se_re^2
  if (var_diff <= 0) return(NA)
  return((b_fe - b_re) / sqrt(var_diff))
}

cat("15.6f: Hausman t-tests\n")
t_exper <- calc_hausman_t(0.0575, 0.0986, 0.0330, 0.0220)
t_south <- calc_hausman_t(-0.3261, -0.2326, 0.1258, 0.0317)
t_union <- calc_hausman_t(0.0822, 0.1027, 0.0312, 0.0245)

cat("t-stat EXPER: ", t_exper, "\n")
cat("t-stat SOUTH: ", t_south, "\n")
cat("t-stat UNION: ", t_union, "\n\n")

# =========================================================
# Exercise 15.20
# =========================================================
cat("--- Exercise 15.20 ---\n")
data("star")

# Create a pdata.frame. The "group" is schid, and the "time" is id.
# This represents clustering of students within schools.
star_p <- pdata.frame(star, index = c("schid", "id"))

# a. Pooled OLS
mod_pool <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = star_p, model = "pooling")
cat("15.20a: Pooled OLS Model\n")
print(summary(mod_pool))

# b. School Fixed Effects
mod_fe <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
              data = star_p, model = "within")
cat("\n15.20b: School Fixed Effects Model\n")
print(summary(mod_fe))

# c. Test for significance of school fixed effects
fe_test <- pFtest(mod_fe, mod_pool)
cat("\n15.20c: F-test for School Fixed Effects\n")
print(fe_test)

# d. School Random Effects
mod_re <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
              data = star_p, model = "random")
cat("\n15.20d: School Random Effects Model\n")
print(summary(mod_re))

# LM test for random effects
lm_test <- plmtest(mod_pool, effect = "individual", type = "bp")
cat("\n15.20d: LM Test for Random Effects\n")
print(lm_test)

# e. Hausman t-tests for coefficients
cat("\n15.20e: Hausman t-tests for specific coefficients\n")
coef_fe <- coef(mod_fe)
coef_re <- coef(mod_re)
se_fe <- sqrt(diag(vcov(mod_fe)))
se_re <- sqrt(diag(vcov(mod_re)))

vars_to_test <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

for (var in vars_to_test) {
  if (var %in% names(coef_fe) && var %in% names(coef_re)) {
    b_fe <- coef_fe[var]
    b_re <- coef_re[var]
    s_fe <- se_fe[var]
    s_re <- se_re[var]
    
    var_diff <- s_fe^2 - s_re^2
    if (var_diff > 0) {
      t_stat <- (b_fe - b_re) / sqrt(var_diff)
      cat(sprintf("Variable: %-12s | t-stat: %8.4f | Significant (5%%): %s\n", 
                  var, t_stat, abs(t_stat) > 1.96))
    } else {
      cat(sprintf("Variable: %-12s | Variance diff <= 0 (Cannot compute t-stat)\n", var))
    }
  }
}

# f. Mundlak Approach
cat("\n15.20f: Mundlak Approach\n")
# Create school-level averages
star_mundlak <- star %>%
  group_by(schid) %>%
  mutate(
    mean_small = mean(small, na.rm = TRUE),
    mean_aide = mean(aide, na.rm = TRUE),
    mean_tchexper = mean(tchexper, na.rm = TRUE),
    mean_boy = mean(boy, na.rm = TRUE),
    mean_white_asian = mean(white_asian, na.rm = TRUE),
    mean_freelunch = mean(freelunch, na.rm = TRUE)
  ) %>%
  ungroup()

# Fit OLS model including the means
mod_mundlak <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                    mean_small + mean_aide + mean_tchexper + mean_boy + mean_white_asian + mean_freelunch,
                  data = star_mundlak)

cat("Mundlak Regression Results (OLS):\n")
print(summary(mod_mundlak))

# Test joint significance of the mean variables
mean_vars <- c("mean_small", "mean_aide", "mean_tchexper", "mean_boy", "mean_white_asian", "mean_freelunch")

mundlak_test <- linearHypothesis(mod_mundlak, mean_vars, test = "F")
cat("\nJoint F-Test for School Averages (Mundlak Test):\n")
print(mundlak_test)
