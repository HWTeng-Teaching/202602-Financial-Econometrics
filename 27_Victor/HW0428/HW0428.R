rm(list=ls())

library(stargazer)
library(tidyverse)
library(POE5Rdata)
library(lmtest)
library(sandwich)
library(gridExtra)
library(AER)
library(car)

# ==========================================
# Question 18
# ==========================================

data(mroz)
df_work <- subset(mroz, lfp == 1)

# (a)
df_work$MOTHERCOLL <- ifelse(df_work$mothereduc > 12, 1, 0)
df_work$FATHERCOLL <- ifelse(df_work$fathereduc > 12, 1, 0)

m_pct <- mean(df_work$MOTHERCOLL) * 100
f_pct <- mean(df_work$FATHERCOLL) * 100

result_18a <- data.frame(
  "Percentage" = c(m_pct, f_pct),
  row.names = c("Mother College (%)", "Father College (%)")
)
stargazer(result_18a, type='text', summary = FALSE, title = "18(a) Parents College Education Proportion")


# (b)
cor_matrix <- cor(df_work[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
stargazer(cor_matrix, type='text', summary = FALSE, title = "18(b) Correlation Matrix")


# (c)
iv_mod_c <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                    MOTHERCOLL + exper + I(exper^2), data = df_work)
stargazer(iv_mod_c, type="text", title="18(c) IV Model (Single IV: MOTHERCOLL)")

conf_c <- confint(iv_mod_c, "educ")
beta_educ_c <- coef(iv_mod_c)["educ"]

result_18c <- data.frame(
  "Estimate" = beta_educ_c,
  "Lower_2.5" = conf_c[1],
  "Upper_97.5" = conf_c[2],
  row.names = c("EDUC 95% CI (Single IV)")
)
stargazer(result_18c, type='text', summary = FALSE, title = "18(c) EDUC Coefficient & Confidence Interval")


# (d)
first_stage_c <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = df_work)
f_val_c <- linearHypothesis(first_stage_c, "MOTHERCOLL = 0")

result_18d <- data.frame(
  "Value" = c(f_val_c$F[2], f_val_c[["Pr(>F)"]][2]),
  row.names = c("F Statistic", "p-value")
)
stargazer(result_18d, type='text', summary = FALSE, title = "18(d) First Stage F-Test (MOTHERCOLL)")


# (e)
iv_mod_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                    MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = df_work)
stargazer(iv_mod_e, type="text", title="18(e) IV Model (Double IV)")

conf_e <- confint(iv_mod_e, "educ")
beta_educ_e <- coef(iv_mod_e)["educ"]

result_18e <- data.frame(
  "Estimate" = beta_educ_e,
  "Lower_2.5" = conf_e[1],
  "Upper_97.5" = conf_e[2],
  row.names = c("EDUC 95% CI (Double IV)")
)
stargazer(result_18e, type='text', summary = FALSE, title = "18(e) EDUC Coefficient & Confidence Interval")


# (f)
first_stage_e <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = df_work)
f_val_e <- linearHypothesis(first_stage_e, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

result_18f <- data.frame(
  "Value" = c(f_val_e$F[2], f_val_e[["Pr(>F)"]][2]),
  row.names = c("Joint F Statistic", "p-value")
)
stargazer(result_18f, type='text', summary = FALSE, title = "18(f) First Stage Joint F-Test")


# (g)
# diagnostics = TRUE 會顯示 Wu-Hausman 與 Sargan 檢定
diag_tests <- summary(iv_mod_e, diagnostics = TRUE)$diagnostics
result_18g <- as.data.frame(diag_tests)
stargazer(result_18g, type='text', summary = FALSE, title = "18(g) Overidentification & Endogeneity Tests")


# ==========================================
# Question 20
# ==========================================

data(capm5)
capm5$y <- capm5$msft - capm5$riskfree
capm5$x <- capm5$mkt - capm5$riskfree

# (a)
model_a <- lm(y ~ x, data = capm5)
stargazer(model_a, type="text", title="20(a) OLS Model")


# (b)
capm5$RANK <- rank(capm5$x)
model_b_stage1 <- lm(x ~ RANK, data = capm5)
stargazer(model_b_stage1, type="text", title="20(b) First Stage (RANK)")


# (c)
capm5$v_hat <- residuals(model_b_stage1)
model_c_test <- lm(y ~ x + v_hat, data = capm5)
stargazer(model_c_test, type="text", title="20(c) Endogeneity Test Regression")


# (d)
model_d_iv <- ivreg(y ~ x | RANK, data = capm5)
stargazer(model_d_iv, type="text", title="20(d) IV Model (RANK)")


# (e)
capm5$POS <- ifelse(capm5$x > 0, 1, 0)
model_e_stage1 <- lm(x ~ RANK + POS, data = capm5)
stargazer(model_e_stage1, type="text", title="20(e) First Stage (RANK + POS)")

f_val_20e <- linearHypothesis(model_e_stage1, c("RANK=0", "POS=0"))

result_20e <- data.frame(
  "Value" = c(f_val_20e$F[2], f_val_20e[["Pr(>F)"]][2]),
  row.names = c("Joint F Statistic", "p-value")
)
stargazer(result_20e, type='text', summary = FALSE, title = "20(e) First Stage Joint F-Test")


# (f)
capm5$v_hat_joint <- residuals(model_e_stage1)
model_f_test <- lm(y ~ x + v_hat_joint, data = capm5)
stargazer(model_f_test, type="text", title="20(f) Endogeneity Test Regression (Joint IV)")


# (g)
model_g_iv <- ivreg(y ~ x | RANK + POS, data = capm5)
stargazer(model_g_iv, type="text", title="20(g) IV Model (RANK + POS)")


# (h)
e_iv <- residuals(model_g_iv)

# Sargan 輔助回歸：將 IV 殘差對「所有」工具變數進行回歸
model_sargan_aux <- lm(e_iv ~ RANK + POS, data = capm5)
r2_sargan <- summary(model_sargan_aux)$r.squared

n_obs <- nrow(capm5)
Sargan_stat <- n_obs * r2_sargan
p_val_sargan <- 1 - pchisq(Sargan_stat, df = 1) # df = 2 (instruments) - 1 (endogenous) = 1

result_20h <- data.frame(
  "Value" = c(Sargan_stat, p_val_sargan),
  row.names = c("Sargan Statistic (LM)", "p-value")
)
stargazer(result_20h, type='text', summary = FALSE, title = "20(h) Sargan Overidentification Test")
