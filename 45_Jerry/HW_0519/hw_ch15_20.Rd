library(POE5Rdata)
library(plm)        # Panel data models (fixed/random effects)
library(lmtest)     # Hypothesis tests
library(car)        # F-tests

data(star)
?star

# ============================================================
# (a) OLS Regression (No Fixed or Random Effects)
# ============================================================

ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star)
summary(ols_model)

# ============================================================
# (b) School Fixed Effects Model
# ============================================================

# Rename 'id' to 'stuid' to avoid conflict with plm's reserved index name 'id'
star2 <- star
star2$stuid <- star2$id
star2$id    <- NULL

# Create panel data object: cross-section = schid, "time" = stuid
pdata <- pdata.frame(star2, index = c("schid", "stuid"))

fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data   = pdata,
                model  = "within",   # Fixed Effects
                effect = "individual")

summary(fe_model)

# Compare OLS vs Fixed Effects coefficients
cat("\n--- OLS Coefficients vs Fixed Effects Coefficients ---\n")
coef_compare <- cbind(
  OLS = coef(ols_model)[-1],   # Drop intercept
  FE  = coef(fe_model)
)
print(round(coef_compare, 4))

# ============================================================
# (c) Test for Significance of School Fixed Effects
# ============================================================

pFtest(fe_model, ols_model)

# ============================================================
# (d) School Random Effects Model + LM Test
# ============================================================

re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data   = pdata,
                model  = "random",
                effect = "individual")

summary(re_model)

# Compare coefficients across all three models
cat("\n--- Coefficient Comparison: OLS vs FE vs RE ---\n")
coef_all <- cbind(
  OLS = coef(ols_model)[-1],
  FE  = coef(fe_model),
  RE  = coef(re_model)[-1]   # Drop intercept
)
print(round(coef_all, 4))

# LM test: H0 = no random effects (Breusch-Pagan LM test)
plmtest(re_model, type = "bp")

# ============================================================
# (e) Hausman Test (FE vs RE) - Using t-statistic from Eq. (15.36)
# ============================================================

hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

vars_of_interest <- c("small", "aide", "tchexper", "white_asian", "freelunch")

b_fe  <- coef(fe_model)[vars_of_interest]
b_re  <- coef(re_model)[vars_of_interest]
se_fe <- sqrt(diag(vcov(fe_model)))[vars_of_interest]
se_re <- sqrt(diag(vcov(re_model)))[vars_of_interest]

t_stat <- (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)

cat("\n--- t-statistics (FE - RE), 5% significance level, critical value ≈ ±1.96 ---\n")
result_e <- data.frame(
  b_FE      = round(b_fe, 4),
  b_RE      = round(b_re, 4),
  t_stat    = round(t_stat, 4),
  reject_H0 = abs(t_stat) > 1.96
)
print(result_e)

# Apply the test separately to BOY
b_fe_boy  <- coef(fe_model)["boy"]
b_re_boy  <- coef(re_model)["boy"]
se_fe_boy <- sqrt(vcov(fe_model)["boy", "boy"])
se_re_boy <- sqrt(vcov(re_model)["boy", "boy"])

t_boy <- (b_fe_boy - b_re_boy) / sqrt(se_fe_boy^2 - se_re_boy^2)
cat("\n--- t-statistic for BOY ---\n")
cat("t =", round(t_boy, 4), "\n")

# ============================================================
# (f) Mundlak Test
# ============================================================

mundlak_vars <- c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")

school_means <- aggregate(star2[, mundlak_vars],
                          by  = list(schid = star2$schid),
                          FUN = mean)
colnames(school_means)[-1] <- paste0(mundlak_vars, "_mean")

star_mundlak <- merge(star2, school_means, by = "schid")

mundlak_model <- lm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
              small_mean + aide_mean + tchexper_mean +
              boy_mean + white_asian_mean + freelunch_mean,
  data = star_mundlak
)

summary(mundlak_model)

# H0: all mean coefficients = 0 (no correlation with unobserved heterogeneity)
mean_vars <- c("small_mean", "aide_mean", "tchexper_mean",
               "boy_mean", "white_asian_mean", "freelunch_mean")

cat("\n--- Mundlak Test: Joint Significance of School-Level Means ---\n")
linearHypothesis(mundlak_model,
                 hypothesis.matrix = mean_vars,
                 test = "Chisq")

cat("\n--- Individual coefficients on school-level means ---\n")
mean_coefs <- summary(mundlak_model)$coefficients[mean_vars, ]
print(round(mean_coefs, 4))