rm(list=ls())
library(POE5Rdata)
data('vacation')

# (a)
model_a = lm(miles~income+age+kids,data = vacation)
summary(model_a)
kids_conf_int = confint(model_a,'kids',level=0.95)
print(kids_conf_int)

# (b)
ols_resid = residuals(model_a)
plot(vacation$income,ols_resid)
abline(h=0,col='red',lwd=2,lty=2)

plot(vacation$age,ols_resid)
abline(h=0,col='red',lwd=2,lty=2)

#(c)
vacation_sorted = vacation[order(vacation$income), ]

data_low = vacation_sorted[1:90, ]
data_high = vacation_sorted[111:200, ]

model_low = lm(miles ~ income + age + kids, data = data_low)
model_high = lm(miles ~ income + age + kids, data = data_high)

sse_low = sum(residuals(model_low)^2)
sse_high = sum(residuals(model_high)^2)

f_stat = sse_high / sse_low

df_group = 90 - 4
f_crit = qf(0.95, df1 = df_group, df2 = df_group)

cat("--- Goldfeld-Quandt Test Results ---\n")
cat("SSE (Low Income):", sse_low, "\n")
cat("SSE (High Income):", sse_high, "\n")
cat("F-Statistic:", f_stat, "\n")
cat("Critical Value (5%):", f_crit, "\n")
#(d)

library(sandwich)
library(lmtest)

robust_ci = coefci(model_a, vcov. = vcovHC(model_a, type = "HC1"), level = 0.95)

kids_robust_ci = robust_ci["kids", ]

cat("--- Confidence Intervals for 'kids' ---\n")
cat("1. Original OLS CI (from part a):\n")
print(kids_conf_int)

cat("\n2. Robust CI (from part d):\n")
print(kids_robust_ci)

#(e)

wls_weights = 1 / (vacation$income^2)

model_gls = lm(miles ~ income + age + kids, data = vacation, weights = wls_weights)

gls_conf_int = confint(model_gls, 'kids', level = 0.95)

gls_robust_ci = coefci(model_gls, vcov. = vcovHC(model_gls, type = "HC1"), level = 0.95)["kids", ]

cat("--- 95% Confidence Intervals for 'KIDS' ---\n")
cat("1. OLS Usual (Part a):       ")
print(kids_conf_int)

cat("2. OLS Robust (Part d):      ")
print(kids_robust_ci)

cat("3. GLS Conventional (Part e):")
print(gls_conf_int)

cat("4. GLS Robust (Part e):      ")
print(gls_robust_ci)




