rm(list=ls())
install.packages("AER")
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
dat$RANK <- rank(dat$MKT_RF, ties.method = "first") #rank()：對數值進行排序，並回傳每個數值的名次（由小到大）
#如果兩天的市場報酬率一模一樣，first 會規定「先出現的那筆資料」排名在前。這確保了產生的 RANK 是一個連續且不重複的整數序列

fs_b <- lm(MKT_RF ~ RANK, data = dat)
summary(fs_b)

F_b <- summary(fs_b)$fstatistic["value"]
cat("First-stage F using RANK =", F_b, "\n")
cat("First-stage R-squared using RANK =", summary(fs_b)$r.squared, "\n")

# (c) Hausman test using first-stage residual from (b)
dat$vhat_rank <- resid(fs_b) #先找到vhat(殘差項的估計值)

hausman_c <- lm(MSFT_RF ~ MKT_RF + vhat_rank, data = dat) #將估計的殘差項當作新的解釋變數代入原先的model，再用LS跑這條新迴歸
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

cat("First-stage R-squared using RANK and POS =", summary(fs_e)$r.squared, "\n")

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

# (h) Manual Sargan test (P.70)
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