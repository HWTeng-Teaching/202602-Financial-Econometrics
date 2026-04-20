rm(list=ls())

library(stargazer)
library(tidyverse)
library(POE5Rdata)
library(lmtest)
library(sandwich)
library(gridExtra)

# ==========================================
# Question 6
# ==========================================

# (a)
var_a_num <- 97161.9174 / (577 - 4)
var_a_den <- 12.024^2
f_stat_a <- var_a_num / var_a_den

f_lower_a <- qf(0.05/2, 577-4, 1000-577-4)
f_upper_a <- qf(1-0.05/2, 577-4, 1000-577-4)

result_6a <- data.frame(
  "Value" = c(f_stat_a, f_lower_a, f_upper_a),
  row.names = c("F Statistic", "F Lower Critical", "F Upper Critical")
)
stargazer(result_6a, type='text', summary = FALSE, title = "6(a) Results")


# (b)
var_b_num <- 100703.0471 / (600 - 5)
var_b_den <- 56231.0382 / (400 - 5)
f_stat_b <- var_b_num / var_b_den

f_crit_b <- qf(1-0.05, 600-5, 400-5)

result_6b <- data.frame(
  "Value" = c(f_stat_b, f_crit_b),
  row.names = c("F Statistic", "F Critical")
)
stargazer(result_6b, type='text', summary = FALSE, title = "6(b) Results")


# (c) & (d)
chi2_c <- qchisq(1-0.05, 4)
chi2_d <- qchisq(1-0.05, 12)

result_6cd <- data.frame(
  "Value" = c(chi2_c, chi2_d),
  row.names = c("Chi-sq (c, df=4)", "Chi-sq (d, df=12)")
)
stargazer(result_6cd, type='text', summary = FALSE, title = "6(c) & 6(d) Critical Values")


# ==========================================
# Question 16
# ==========================================

data(vacation)

# (a)
model_ols <- lm(miles ~ income + age + kids, data = vacation)
stargazer(model_ols, type="text", title="OLS Model (16a)")

conf_a <- confint(model_ols, "kids", level = 0.95)

result_16a <- data.frame(
  "Lower_2.5" = conf_a[1],
  "Upper_97.5" = conf_a[2],
  row.names = c("KIDS 95% CI (OLS)")
)
stargazer(result_16a, type='text', summary = FALSE, title = "16(a) OLS Confidence Interval")


# (b) 殘差圖診斷
vacation <- vacation %>%
  mutate(resid_ols = resid(model_ols))

p1 <- ggplot(vacation, aes(x = income, y = resid_ols)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs INCOME", x = "Income", y = "Residuals") +
  theme_minimal()

p2 <- ggplot(vacation, aes(x = age, y = resid_ols)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs AGE", x = "Age", y = "Residuals") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)
# 觀察：若殘差隨 INCOME 增加而呈「漏斗狀」擴散，則存在異質變異數。


# (c) Goldfeld-Quandt 檢定
gq_test <- gqtest(model_ols, fraction = 20, order.by = ~ income, data = vacation)

gq_table <- data.frame(
  "Value" = c(gq_test$statistic, gq_test$parameter[1], gq_test$parameter[2], gq_test$p.value),
  row.names = c("GQ Statistic", "df1 (numerator)", "df2 (denominator)", "p-value")
)
stargazer(gq_table, type = "text", summary = FALSE, title = "16(c) Goldfeld-Quandt Test")


# (d) White 穩健標準誤差 (Heteroskedasticity Robust SE)
robust_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
stargazer(robust_se, type = "text", title = "16(d) Robust Standard Errors")

se_kids_robust <- sqrt(vcovHC(model_ols, type = "HC1")["kids", "kids"])
beta_kids <- coef(model_ols)["kids"]
df_deg <- df.residual(model_ols)

conf_d_lower <- beta_kids - qt(0.975, df_deg) * se_kids_robust
conf_d_upper <- beta_kids + qt(0.975, df_deg) * se_kids_robust

result_16d <- data.frame(
  "Lower_2.5" = conf_d_lower,
  "Upper_97.5" = conf_d_upper,
  row.names = c("KIDS 95% CI (Robust)")
)
stargazer(result_16d, type='text', summary = FALSE, title = "16(d) Robust Confidence Interval")


# (e) GLS 估計 (假設變異數正比於 INCOME^2)
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = 1/(income^2))
stargazer(model_gls, type="text", title="GLS Model (16e)")

# GLS 傳統信賴區間
conf_e_trad <- confint(model_gls, "kids", level = 0.95)

# GLS 穩健信賴區間 (Robust GLS)
se_kids_gls_robust <- sqrt(vcovHC(model_gls, type = "HC1")["kids", "kids"])
beta_kids_gls <- coef(model_gls)["kids"]

conf_e_robust_lower <- beta_kids_gls - qt(0.975, df_deg) * se_kids_gls_robust
conf_e_robust_upper <- beta_kids_gls + qt(0.975, df_deg) * se_kids_gls_robust

result_16e <- data.frame(
  "Lower_2.5" = c(conf_e_trad[1], conf_e_robust_lower),
  "Upper_97.5" = c(conf_e_trad[2], conf_e_robust_upper),
  row.names = c("KIDS CI (GLS Trad)", "KIDS CI (GLS Robust)")
)
stargazer(result_16e, type='text', summary = FALSE, title = "16(e) GLS Confidence Intervals")