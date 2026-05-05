rm(list=ls())
library(AER)
library(car)
library(POE5Rdata)
data('mroz')

mroz1 = mroz[mroz$lfp == 1, ]

# a.
mroz1$MOTHERCOLL = ifelse(mroz1$mothereduc > 12, 1, 0)
mroz1$FATHERCOLL = ifelse(mroz1$fathereduc > 12, 1, 0)

pct_mother_coll = mean(mroz1$MOTHERCOLL) * 100
pct_father_coll = mean(mroz1$FATHERCOLL) * 100

cat("Percentage of mothers with some college education:", pct_mother_coll, "%\n")
cat("Percentage of fathers with some college education:", pct_father_coll, "%\n")

#b.
cor_matrix = cor(mroz1[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print(cor_matrix)


#c
model_c = ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                   MOTHERCOLL + exper + I(exper^2), 
                 data = mroz1)

summary(model_c)

confint(model_c, "educ", level = 0.95)

#d
first_stage_d = lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = mroz1)

summary(first_stage_d)

linearHypothesis(first_stage_d, "MOTHERCOLL = 0")
#e

model_e = ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                   MOTHERCOLL + FATHERCOLL + exper + I(exper^2), 
                 data = mroz1)

summary(model_e)

confint(model_e, "educ", level = 0.95)

#f

first_stage_f = lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz1)

summary(first_stage_f)

linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))
#g

summary(model_e, diagnostics = TRUE)


#10.20
data('capm5')
#a.
capm5$msft_rp = capm5$msft - capm5$riskfree
capm5$mkt_rp = capm5$mkt - capm5$riskfree

capm_ols = lm(msft_rp ~ mkt_rp, data = capm5)

summary(capm_ols)

# b.

capm5$RANK = rank(capm5$mkt_rp)

first_stage_b = lm(mkt_rp ~ RANK, data = capm5)

summary(first_stage_b)
# c.

capm5$v_hat = residuals(first_stage_b)

augmented_model = lm(msft_rp ~ mkt_rp + v_hat, data = capm5)

summary(augmented_model)
# d.

capm_iv = ivreg(msft_rp ~ mkt_rp | RANK, data = capm5)

summary(capm_iv)
# e.

capm5$POS = ifelse(capm5$mkt_rp > 0, 1, 0)

first_stage_e = lm(mkt_rp ~ RANK + POS, data = capm5)

summary(first_stage_e)

# f.

capm5$v_hat_e = residuals(first_stage_e)

augmented_model_f = lm(msft_rp ~ mkt_rp + v_hat_e, data = capm5)

summary(augmented_model_f)

# g.

capm_iv_g = ivreg(msft_rp ~ mkt_rp | RANK + POS, data = capm5)

summary(capm_iv_g)
# h.

# the IV residuals from the part (g) model
capm5$e_iv = residuals(capm_iv_g)

# the auxiliary regression (residuals on all instruments)
sargan_aux = lm(e_iv ~ RANK + POS, data = capm5)

# Sargan test statistic (N * R^2)
N = nobs(sargan_aux)
r_squared_aux = summary(sargan_aux)$r.squared
sargan_stat = N * r_squared_aux

# the p-value (Chi-square distribution with df = 1)
p_value = 1 - pchisq(sargan_stat, df = 1)

cat("Sargan Statistic:", sargan_stat, "\nP-value:", p_value, "\n")
















