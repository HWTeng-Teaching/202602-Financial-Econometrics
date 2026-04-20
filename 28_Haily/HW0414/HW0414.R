library(POE5Rdata)
data("vacation")
library(stargazer)
install.packages(c("lmtest", "sandwich"))
library(lmtest)
library(sandwich)

# 8.16(a)
mod_a = lm(miles ~ income + age + kids, data = vacation)
stargazer(mod_a, type="text", title="OLS Estimates")

ci_a = confint(mod_a, "kids", level = 0.95)

table_a = data.frame(Variable = "kids",
                     Lower_95 = ci_a[1],
                     Upper_95 = ci_a[2])
stargazer(table_a, type = "text", summary = FALSE, title="95% CI for KIDS (OLS)")

# 8.16(b)
par(mfrow = c(1,2))
plot(vacation$income, resid(mod_a), pch=16, col="steelblue",
     xlab="INCOME", ylab="Residuals", main="Residuals vs INCOME")
abline(h=0, lwd=2)
plot(vacation$age, resid(mod_a), pch=16, col="steelblue",
     xlab="AGE", ylab="Residuals", main="Residuals vs AGE")
abline(h=0, lwd=2)

# 8.16(c)
vacation_sorted = vacation[order(vacation$income), ]

mod_c_first = lm(miles ~ income + age + kids, data = vacation_sorted[1:90, ])
mod_c_last = lm(miles ~ income + age + kids, data = vacation_sorted[111:200, ])

rss_first = sum(resid(mod_c_first)^2)
rss_last = sum(resid(mod_c_last)^2)

df_1 = 86
df_2 = 86
gq_stat = rss_last / rss_first
p_value_gq = 1 - pf(gq_stat, df_1, df_2)

table_c = data.frame(Test = "Goldfeld-Quandt",
                     GQ_Statistic = gq_stat,
                     p_value = p_value_gq)
stargazer(table_c, type = "text", summary = FALSE, title="Goldfeld-Quandt Test Results")

# 8.16(d)
robust_se_a = vcovHC(mod_a, type="HC1")

b_kids = coef(mod_a)["kids"]
se_kids_rob = sqrt(diag(robust_se_a))["kids"]
t_crit = qt(0.975, mod_a$df.residual)

ci_d_lower = b_kids - t_crit * se_kids_rob
ci_d_upper = b_kids + t_crit * se_kids_rob

table_d = data.frame(Model = c("OLS Default CI", "OLS Robust CI"),
                     Lower_95 = c(ci_a[1], ci_d_lower),
                     Upper_95 = c(ci_a[2], ci_d_upper))
stargazer(table_d, type = "text", summary = FALSE, title="Comparison of OLS Confidence Intervals")

# 8.16(e)
mod_e = lm(miles ~ income + age + kids, data = vacation, weights = 1/(income^2))

ci_e_conv = confint(mod_e, "kids", level = 0.95)

robust_se_e = vcovHC(mod_e, type="HC1")
b_kids_gls = coef(mod_e)["kids"]
se_kids_gls_rob = sqrt(diag(robust_se_e))["kids"]

ci_e_rob_lower = b_kids_gls - t_crit * se_kids_gls_rob
ci_e_rob_upper = b_kids_gls + t_crit * se_kids_gls_rob

table_e = data.frame(Model = c("OLS Robust (from d)", "GLS Conventional", "GLS Robust"),
                     Lower_95 = c(ci_d_lower, ci_e_conv[1], ci_e_rob_lower),
                     Upper_95 = c(ci_d_upper, ci_e_conv[2], ci_e_rob_upper))
stargazer(table_e, type = "text", summary = FALSE, title="GLS vs OLS Confidence Intervals")