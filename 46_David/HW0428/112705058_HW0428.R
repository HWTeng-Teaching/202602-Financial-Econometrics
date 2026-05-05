library(AER)
library(lmtest)
library(sandwich)

# =====================================================
# 10.18 mroz - Parent's college as IV
# =====================================================
url1 <- "http://www.principlesofeconometrics.com/poe5/data/csv/mroz.csv"
mroz <- read.csv(url1)

# only married women in labor force (428 obs)
mroz <- subset(mroz, lfp == 1)

#a
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

pct_mother <- mean(mroz$mothercoll) * 100
pct_father <- mean(mroz$fathercoll) * 100

cat("MOTHERCOLL %:", pct_mother, "\n")
cat("FATHERCOLL %:", pct_father, "\n")

#b
cor_matrix <- cor(mroz[, c("educ", "mothercoll", "fathercoll", "mothereduc", "fathereduc")])
print(round(cor_matrix, 4))

#c  IV: MOTHERCOLL only
mroz$lwage <- log(mroz$wage)

iv_c <- ivreg(lwage ~ educ + exper + I(exper^2) |
                mothercoll + exper + I(exper^2), data = mroz)

summary(iv_c)
ci_c <- confint(iv_c, "educ", level = 0.95)
cat("\n(c) 95% CI for EDUC (IV = MOTHERCOLL):\n")
print(ci_c)

#d  first-stage for (c)
fs_d <- lm(educ ~ mothercoll + exper + I(exper^2), data = mroz)
summary(fs_d)

# F-test: MOTHERCOLL = 0
fs_d_restricted <- lm(educ ~ exper + I(exper^2), data = mroz)
f_test_d <- anova(fs_d_restricted, fs_d)
cat("\n(d) F-statistic for MOTHERCOLL:\n")
print(f_test_d)

#e  IV: MOTHERCOLL + FATHERCOLL
iv_e <- ivreg(lwage ~ educ + exper + I(exper^2) |
                mothercoll + fathercoll + exper + I(exper^2), data = mroz)

summary(iv_e)
ci_e <- confint(iv_e, "educ", level = 0.95)
cat("\n(e) 95% CI for EDUC (IV = MOTHERCOLL + FATHERCOLL):\n")
print(ci_e)

cat("\n區間寬度比較:\n")
cat("(c) width:", diff(ci_c[1, ]), "\n")
cat("(e) width:", diff(ci_e[1, ]), "\n")

#f  first-stage for (e), joint significance
fs_f <- lm(educ ~ mothercoll + fathercoll + exper + I(exper^2), data = mroz)
summary(fs_f)

fs_f_restricted <- lm(educ ~ exper + I(exper^2), data = mroz)
f_test_f <- anova(fs_f_restricted, fs_f)
cat("\n(f) Joint F-test for MOTHERCOLL + FATHERCOLL:\n")
print(f_test_f)

#g  Sargan test for surplus instrument
# residuals from IV (e), regress on all exogenous variables
res_iv_e <- residuals(iv_e)
sargan_aux <- lm(res_iv_e ~ mothercoll + fathercoll + exper + I(exper^2), data = mroz)

n <- nrow(mroz)
r2_sargan <- summary(sargan_aux)$r.squared
sargan_stat <- n * r2_sargan
# df = #IV - #endogenous = 2 - 1 = 1
p_sargan <- pchisq(sargan_stat, df = 1, lower.tail = FALSE)

cat("\n(g) Sargan test:\n")
cat("N*R^2 =", sargan_stat, "\n")
cat("p-value (chi^2 df=1):", p_sargan, "\n")

# automatic test
cat("\nDiagnostic tests from ivreg:\n")
print(summary(iv_e, diagnostics = TRUE)$diagnostics)


# =====================================================
# 10.20 capm5 - CAPM with measurement error
# =====================================================
url2 <- "http://www.principlesofeconometrics.com/poe5/data/csv/capm5.csv"
capm <- read.csv(url2)

# risk premium
capm$msft_rp <- capm$msft - capm$riskfree
capm$mkt_rp  <- capm$mkt  - capm$riskfree

#a  OLS
ols_a <- lm(msft_rp ~ mkt_rp, data = capm)
summary(ols_a)
cat("\n(a) Microsoft beta (OLS):", coef(ols_a)["mkt_rp"], "\n")

#b  RANK as IV - first stage
capm$rank_mkt <- rank(capm$mkt_rp)

fs_b <- lm(mkt_rp ~ rank_mkt, data = capm)
summary(fs_b)

cat("\n(b) First-stage R^2:", summary(fs_b)$r.squared, "\n")
cat("F-stat for RANK:", summary(fs_b)$fstatistic[1], "\n")

#c  Hausman-style test using first-stage residuals
v_hat <- residuals(fs_b)
aug_c <- lm(msft_rp ~ mkt_rp + v_hat, data = capm)
summary(aug_c)

cat("\n(c) Augmented regression - test v_hat at 1%:\n")
print(coeftest(aug_c)["v_hat", ])

#d  IV/2SLS using RANK
iv_d <- ivreg(msft_rp ~ mkt_rp | rank_mkt, data = capm)
summary(iv_d)

cat("\n(d) Beta comparison:\n")
cat("OLS beta :", coef(ols_a)["mkt_rp"], "\n")
cat("IV  beta :", coef(iv_d)["mkt_rp"], "\n")

#e  RANK + POS as IV
capm$pos <- ifelse(capm$mkt_rp > 0, 1, 0)

fs_e <- lm(mkt_rp ~ rank_mkt + pos, data = capm)
summary(fs_e)

# joint significance of IV
fs_e_restricted <- lm(mkt_rp ~ 1, data = capm)
f_test_e <- anova(fs_e_restricted, fs_e)
cat("\n(e) Joint F-test for RANK + POS:\n")
print(f_test_e)
cat("First-stage R^2:", summary(fs_e)$r.squared, "\n")

#f  Hausman test using residuals from (e)
v_hat_e <- residuals(fs_e)
aug_f <- lm(msft_rp ~ mkt_rp + v_hat_e, data = capm)
summary(aug_f)

cat("\n(f) Hausman test - v_hat coefficient at 1%:\n")
print(coeftest(aug_f)["v_hat_e", ])

#g  IV/2SLS using RANK + POS
iv_g <- ivreg(msft_rp ~ mkt_rp | rank_mkt + pos, data = capm)
summary(iv_g)

cat("\n(g) Beta comparison:\n")
cat("OLS beta      :", coef(ols_a)["mkt_rp"], "\n")
cat("IV beta (RANK+POS):", coef(iv_g)["mkt_rp"], "\n")

#h  Sargan test - manual
res_iv_g <- residuals(iv_g)
sargan_aux_h <- lm(res_iv_g ~ rank_mkt + pos, data = capm)

n2 <- nrow(capm)
r2_h <- summary(sargan_aux_h)$r.squared
sargan_h <- n2 * r2_h
# df = #IV - #endogenous = 2 - 1 = 1
p_sargan_h <- pchisq(sargan_h, df = 1, lower.tail = FALSE)

cat("\n(h) Sargan test (manual):\n")
cat("N*R^2 =", sargan_h, "\n")
cat("p-value (chi^2 df=1):", p_sargan_h, "\n")
cat("5% critical value:", qchisq(0.95, df = 1), "\n")
