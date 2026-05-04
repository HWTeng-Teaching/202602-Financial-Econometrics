install.packages(c("AER", "car", "stargazer"))
library(AER)
library(car)
library(stargazer)

library(POE5Rdata)
data("mroz")
mroz_work <- subset(mroz, lfp == 1)

# 10.18(a)
mroz_work$MOTHERCOLL = ifelse(mroz_work$mothereduc > 12, 1, 0)
mroz_work$FATHERCOLL = ifelse(mroz_work$fathereduc > 12, 1, 0)

pct_mother = mean(mroz_work$MOTHERCOLL) * 100
pct_father = mean(mroz_work$FATHERCOLL) * 100

table_a = data.frame(Parent = c("Mother", "Father"),
                     Percentage_with_College = c(pct_mother, pct_father))
stargazer(table_a, type = "text", summary = FALSE, title = "Percentage of Parents with College")

# 10.18(b)
cor_matrix = cor(mroz_work[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")
stargazer(cor_matrix, type = "text", title = "Correlation Matrix")

# 10.18(c)
iv_mod_c = ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + MOTHERCOLL , data = mroz_work)

ci_c = confint(iv_mod_c, "educ", level = 0.95)
table_c = data.frame(Model = "IV (MOTHERCOLL)",
                     Lower_95 = ci_c[1], Upper_95 = ci_c[2])
stargazer(table_c, type = "text", summary = FALSE, title = "95% CI for EDUC")

# 10.18(d)
first_stage_d = lm(educ ~ exper + I(exper^2) +  MOTHERCOLL , data = mroz_work)

f_test_d = linearHypothesis(first_stage_d, "MOTHERCOLL = 0")
f_stat_d = f_test_d$F[2]

table_d = data.frame(Test = "F-test for MOTHERCOLL", F_statistic = f_stat_d)
stargazer(table_d, type = "text", summary = FALSE, title = "First-Stage F-test Result")

# 10.18(e)
iv_mod_e = ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + MOTHERCOLL + FATHERCOLL , data = mroz_work)

ci_e = confint(iv_mod_e, "educ", level = 0.95)
table_e = data.frame(Model = c("IV (Mother)", "IV (Both)"),
                     Lower_95 = c(ci_c[1], ci_e[1]),
                     Upper_95 = c(ci_c[2], ci_e[2]))
stargazer(table_e, type = "text", summary = FALSE, title = "95% CI Comparison")

# 10.18(f)
first_stage_f = lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL , data = mroz_work)

f_test_f = linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))
f_stat_f = f_test_f$F[2]

table_f = data.frame(Test = "Joint F-test (Both IVs)", F_statistic = f_stat_f)
stargazer(table_f, type = "text", summary = FALSE, title = "Joint First-Stage F-test")

# 10.18(g)
iv_diagnostics = summary(iv_mod_e, diagnostics = TRUE)$diagnostics
sargan_p = iv_diagnostics["Sargan", "p-value"]

table_g = data.frame(Test = "Sargan (Overidentification)", p_value = sargan_p)
stargazer(table_g, type = "text", summary = FALSE, title = "Surplus Instrument Validity")


data("capm5")

capm5$y = capm5$msft - capm5$riskfree  
capm5$x = capm5$mkt - capm5$riskfree

# 10.20(a)
mod_a = lm(y ~ x, data = capm5)
beta_ols = coef(mod_a)["x"]
stargazer(mod_a, type = "text", title = "OLS CAPM")

# 10.20(b)
capm5$RANK = rank(capm5$x)

mod_b_first = lm(x ~ RANK, data = capm5)
r2_b = summary(mod_b_first)$r.squared
pval_rank = summary(mod_b_first)$coefficients["RANK", 4]

table_b = data.frame(Variable = "RANK", R_squared = r2_b, p_value = pval_rank)
stargazer(table_b, type = "text", summary = FALSE, title = "First-Stage Results (RANK)")

# 10.20(c)
capm5$vhat_c = resid(mod_b_first)
mod_c_aug = lm(y ~ x + vhat_c, data = capm5)

pval_vhat_c = summary(mod_c_aug)$coefficients["vhat_c", 4]
table_c = data.frame(Test = "Significance of v_hat (Exogeneity)", p_value = pval_vhat_c)
stargazer(table_c, type = "text", summary = FALSE, title = "Augmented Regression Test")

# 10.20(d)
mod_d_iv = ivreg(y ~ x | RANK, data = capm5)
beta_iv_d = coef(mod_d_iv)["x"]

stargazer(mod_a, mod_d_iv, type = "text", title = "OLS vs IV (RANK)", 
          column.labels = c("OLS", "IV (RANK)"))

# 10.20(e)
capm5$POS = ifelse(capm5$x > 0, 1, 0)

mod_e_first = lm(x ~ RANK + POS, data = capm5)
r2_e = summary(mod_e_first)$r.squared

f_test_e = linearHypothesis(mod_e_first, c("RANK = 0", "POS = 0"))
f_stat_e = f_test_e$F[2]

table_e = data.frame(Test = "Joint F-test (RANK, POS)", F_statistic = f_stat_e, R_squared = r2_e)
stargazer(table_e, type = "text", summary = FALSE, title = "First-Stage Results (RANK & POS)")

# 10.20(f)
capm5$vhat_e = resid(mod_e_first)
mod_f_aug = lm(y ~ x + vhat_e, data = capm5)

pval_vhat_e = summary(mod_f_aug)$coefficients["vhat_e", 4]
table_f = data.frame(Test = "Significance of v_hat_e", p_value = pval_vhat_e)
stargazer(table_f, type = "text", summary = FALSE, title = "Augmented Regression Test (Both IVs)")

# 10.20(g)
mod_g_iv = ivreg(y ~ x | RANK + POS, data = capm5)

stargazer(mod_a, mod_g_iv, type = "text", title = "OLS vs IV (Both)", 
          column.labels = c("OLS", "IV (Both)"))

# 10.20(h)
capm5$iv_resid_g = resid(mod_g_iv)

mod_h_sargan = lm(iv_resid_g ~ RANK + POS, data = capm5)

N = nrow(capm5)
r2_h = summary(mod_h_sargan)$r.squared
sargan_stat = N * r2_h

sargan_pval = 1 - pchisq(sargan_stat, df = 1)

table_h = data.frame(Test = "Manual Sargan Test", Statistic = sargan_stat, p_value = sargan_pval)
stargazer(table_h, type = "text", summary = FALSE, title = "Surplus Instrument Validity")