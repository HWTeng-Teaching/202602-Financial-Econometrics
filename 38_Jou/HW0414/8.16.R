library(PoEdata)
data("vacation")

# (a) OLS
mod <- lm(miles ~ income + age + kids, data = vacation)
summary(mod)
confint(mod, "kids", level = 0.95)

# (b) residual plots
plot(vacation$income, resid(mod), xlab = "Income", ylab = "Residuals", main = "Residuals vs Income")
abline(h = 0, lty = 2)

plot(vacation$age, resid(mod), xlab = "Age", ylab = "Residuals", main = "Residuals vs Age")
abline(h = 0, lty = 2)

# (c) Goldfeld-Quandt
vac_sorted <- vacation[order(vacation$income), ]
low  <- vac_sorted[1:90, ]
high <- vac_sorted[111:200, ]

mod_low  <- lm(miles ~ income + age + kids, data = low)
mod_high <- lm(miles ~ income + age + kids, data = high)

sse_low  <- sum(resid(mod_low)^2)
sse_high <- sum(resid(mod_high)^2)

Fstat <- (sse_high/(90-4)) / (sse_low/(90-4))
Fstat
qf(0.95, 86, 86)

# (d) robust SE
library(sandwich) # 用於 vcovHC (計算 HC1)
library(lmtest) # 用於 coeftest (穩健標準誤)

coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
beta_kids <- coef(mod)["kids"]
se_kids_r <- sqrt(vcovHC(mod, type = "HC1")["kids","kids"])
beta_kids + c(-1,1)*qt(0.975, df=196)*se_kids_r

# (e) GLS / WLS
mod_gls <- lm(miles ~ income + age + kids, data = vacation, weights = 1/(income^2))
summary(mod_gls)

confint(mod_gls, "kids", level = 0.95)

coeftest(mod_gls, vcov = vcovHC(mod_gls, type = "HC1"))
beta_kids_gls <- coef(mod_gls)["kids"]
se_kids_gls_r <- sqrt(vcovHC(mod_gls, type = "HC1")["kids","kids"])
beta_kids_gls + c(-1,1)*qt(0.975, df=196)*se_kids_gls_r