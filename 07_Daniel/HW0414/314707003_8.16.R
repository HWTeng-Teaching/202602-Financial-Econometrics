rm(list=ls())
library(POE5Rdata)

#install.packages("car")
library(lmtest)
library(car)
#8.16
data = vacation

#(a)
alpha = 0.05
mod1 = lm(miles ~ income + age + kids ,data = vacation)
smod1 = summary(mod1)
df = df.residual(mod1)
b4 = coef(mod1)[[4]]
tc = qt(1-alpha/2, df)
se_b4 = coef(smod1)[4,2]
uc = b4 + tc * se_b4
lc = b4 - tc * se_b4
cat(
  "8.16(a)", "\n",
  "the 95% CI is : [",lc,",",uc,"]", "\n",
  sep=""
)

#(b)
par(mfrow = c(1, 2))
plot(vacation$income, resid(mod1),
     main = "Residuals vs INCOME",
     xlab = "income", ylab = "Residuals")
abline(h = 0, lty = 2)
plot(vacation$age, resid(mod1),
     main = "Residuals vs AGE",
     xlab = "age", ylab = "Residuals")
abline(h = 0, lty = 2)

#(c)
vacation_sorted = vacation[order(vacation$income), ]
low90  = vacation_sorted[1:90, ]
high90 = vacation_sorted[111:200, ]

mod_low  <- lm(miles ~ income + age + kids, data = low90)
mod_high <- lm(miles ~ income + age + kids, data = high90)

ssr_low  <- sum(resid(mod_low)^2)
ssr_high <- sum(resid(mod_high)^2)
df_low = df.residual(mod_high)
df_high = df.residual(mod_low)
F_stat <- (ssr_high / df_high) / (ssr_low / df_low)
tc_c = qf(0.95, df_high, df_low)
cat(
  "8.16(c)", "\n",
  "the F* is :  ",F_stat, "\n",
  "and F_c is : ",tc_c, "\n",
  sep=""
)

#(d)
mod1_hc = hccm(mod1,type = "hc1")
vac.HC1 = coeftest(mod1, vcov.=mod1_hc)
se_b4_rob = vac.HC1[4,2]
uc_rob = b4 + tc * se_b4_rob
lc_rob = b4 - tc * se_b4_rob

cat(
  "8.16(d)", "\n",
  "the 95% CI using heteroskedasticity robust standard errors is : [",lc_rob,",",uc_rob,"]", "\n",
  sep=""
)

#(e)
w = 1/vacation$income
mod1.wls = lm(miles ~ income + age + kids, weights=w, data=vacation)
smod1.wls = summary(mod1.wls)
se_b4.wls = coef(smod1.wls)[4,2]
b4.wls = coef(mod1.wls)[[4]]
uc.wls = b4.wls + tc * se_b4.wls
lc.wls = b4.wls - tc * se_b4.wls

# robust
mod1.wls_hc = hccm(mod1.wls ,type = "hc1")
vac.wls.HC1 = coeftest(mod1.wls, vcov.=mod1.wls_hc)
se_b4.wls_rob = vac.wls.HC1[4,2]
uc_rob.wls = b4.wls + tc * se_b4.wls_rob
lc_rob.wls = b4.wls - tc * se_b4.wls_rob

cat(
  "8.16(e)", "\n",
  "the 95% CI using conventional GLS standard errors is : [",lc.wls,",",uc.wls,"]", "\n",
  "the 95% CI using  robust conventional GLS standard errors is : [",lc_rob.wls,",",uc_rob.wls,"]", "\n",
  sep=""
)
