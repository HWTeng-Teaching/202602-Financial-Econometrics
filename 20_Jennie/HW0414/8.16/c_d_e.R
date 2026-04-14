load("D:/碩一下/計量經濟/作業/HW0414/8.16/vacation.rdata")
model =lm(miles ~ income + age + kids, data = vacation)
summary(model)

#c
vacation_sorted=vacation[order(vacation$income), ]

low_income = vacation_sorted[1:90, ]
high_income= vacation_sorted[(nrow(vacation_sorted)-89):nrow(vacation_sorted), ]

model_low=lm(miles ~ income + age + kids, data = low_income)
model_high= lm(miles ~ income + age + kids, data = high_income)

SSE_low=sum(residuals(model_low)^2)
SSE_high = sum(residuals(model_high)^2)

df_low = 90 - 4
df_high= 90 - 4

F_stat = (SSE_high / df_high) / (SSE_low / df_low)
F_stat

qf(0.95, df_high, df_low)

#d
library(sandwich)
library(lmtest)

robust_vcov = vcovHC(model, type = "HC1") 
coeftest(model, vcov = robust_vcov)

coef_ci =coefci(model, vcov = robust_vcov, level = 0.95)
print(coef_ci["kids", ])

#e

vacation$w_e = 1 / (vacation$income^2)
model_gls_e = lm(miles ~ kids + age + income, 
                 data = vacation, 
                 weights = w_e)

ci_conv_e <- confint(model_gls_e, "kids", level = 0.95)

library(sandwich)
library(lmtest)
robust_vcov_e <- vcovHC(model_gls_e, type = "HC1")
ci_robust_e <- coefci(model_gls_e, vcov = robust_vcov_e, level = 0.95, parm = "kids")

print(ci_conv_e)
print(ci_robust_e)