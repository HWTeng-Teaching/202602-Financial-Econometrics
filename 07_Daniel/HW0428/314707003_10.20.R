rm(list=ls())
#install.packages("AER")
library(POE5Rdata)
library(AER)
library(car)

data("capm5")
dat = capm5

dat$MSFT_RF <- dat$msft - dat$riskfree
dat$MKT_RF  <- dat$mkt  - dat$riskfree

# (a) OLS CAPM
ols_a <- lm(MSFT_RF ~ MKT_RF, data = dat)
summary(ols_a)
confint(ols_a)

beta_ols <- coef(ols_a)["MKT_RF"]
cat("OLS beta =", beta_ols, "\n")

# (b) RANK and first stage
dat$RANK <- rank(dat$MKT_RF, ties.method = "first")

fs_b <- lm(MKT_RF ~ RANK, data = dat)
summary(fs_b)

F_b <- summary(fs_b)$fstatistic["value"]
cat("First-stage F using RANK =", F_b, "\n")
cat("First-stage R2 using RANK =", summary(fs_b)$r.squared, "\n")

# (c) Hausman test using first-stage residual from (b)
dat$vhat_rank <- resid(fs_b)

hausman_c <- lm(MSFT_RF ~ MKT_RF + vhat_rank, data = dat)
summary(hausman_c)

p_c <- summary(hausman_c)$coefficients["vhat_rank", "Pr(>|t|)"]
cat("Hausman p-value using RANK =", p_c, "\n")

# (d) IV using RANK
iv_d <- ivreg(
  MSFT_RF ~ MKT_RF |
    RANK,
  data = dat
)

summary(iv_d)
confint(iv_d)

cat("OLS beta =", coef(ols_a)["MKT_RF"], "\n")
cat("IV beta using RANK =", coef(iv_d)["MKT_RF"], "\n")

# (e) POS and first stage using RANK and POS
dat$POS <- ifelse(dat$MKT_RF > 0, 1, 0)

fs_e <- lm(MKT_RF ~ RANK + POS, data = dat)
summary(fs_e)

test_e <- linearHypothesis(
  fs_e,
  c("RANK = 0", "POS = 0")
)

test_e

cat("First-stage R2 using RANK and POS =", summary(fs_e)$r.squared, "\n")

# (f) Hausman test using first-stage residual from (e)
dat$vhat_rank_pos <- resid(fs_e)

hausman_f <- lm(MSFT_RF ~ MKT_RF + vhat_rank_pos, data = dat)
summary(hausman_f)

p_f <- summary(hausman_f)$coefficients["vhat_rank_pos", "Pr(>|t|)"]
cat("Hausman p-value using RANK and POS =", p_f, "\n")

# (g) IV using RANK and POS
iv_g <- ivreg(
  MSFT_RF ~ MKT_RF |
    RANK + POS,
  data = dat
)

summary(iv_g)
confint(iv_g)

cat("OLS beta =", coef(ols_a)["MKT_RF"], "\n")
cat("IV beta using RANK =", coef(iv_d)["MKT_RF"], "\n")
cat("IV beta using RANK and POS =", coef(iv_g)["MKT_RF"], "\n")

# (h) Manual Sargan test
dat$uhat_iv_g <- resid(iv_g)

sargan_reg <- lm(uhat_iv_g ~ RANK + POS, data = dat)
summary(sargan_reg)

n <- nrow(dat)
R2_sargan <- summary(sargan_reg)$r.squared
Sargan_stat <- n * R2_sargan

df_sargan <- 2 - 1
p_sargan <- 1 - pchisq(Sargan_stat, df = df_sargan)

cat("Sargan statistic =", Sargan_stat, "\n")
cat("Sargan df =", df_sargan, "\n")
cat("Sargan p-value =", p_sargan, "\n")

if (p_sargan < 0.05) {
  cat("Reject surplus IV validity at 5% level.\n")
} else {
  cat("Do not reject surplus IV validity at 5% level.\n")
}  