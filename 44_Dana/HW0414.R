#8.16 

#a
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"))
m1 <- lm(miles ~ income + age + kids, data = vacation)
summary(m1)
confint(m1,"kids", level = 0.95)

#b
plot(vacation$income, resid(m1))
abline(h=0, lty=2)

plot(vacation$age, resid(m1))
abline(h=0, lty=2)

#c
vac_sort <-vacation[order(vacation$income),]
low90 <- vac_sort[1:90,]
high90 <- vac_sort[111:200, ]
m_low <- lm(miles ~ income + age + kids, data = low90 )
m_high <- lm(miles ~ income + age + kids, data = high90 )
df_high <- 90-4
df_low <- 90-4
sse_low <- sum(resid(m_low)^2)
sse_high <- sum(resid(m_high)^2)
F <- (sse_high/df_high)/ (sse_low/df_low)
f_Crit <- qf(0.95, df_high, df_low)

#d
library(lmtest)
library(sandwich)

rb_vcov <-(vcovHC(m1, type ="HC1"))
coeftest(m1, vcoc=rb_vcov)

b_kids <- coef(m1)["kids"]
se_kids_rb <- sqrt(rb_vcov["kids","kids"])
t_crit <-qt(0.975, df=df.residual(m1))
ci_kids_rb <- c(b_kids - t_crit*se_kids_rb,
                b_kids + t_crit*se_kids_rb)
ci_kids_rb

#e

m_gls <- lm(miles ~ income + age + kids, data = vacation, weight=1/(income^2))
summary(m_gls)
confint(m_gls, "kids", level =0.95) #GLS_CI

gls_rb_vcov <-(vcovHC(m_gls, type ="HC1"))
coeftest(m_gls, vcoc=gls_rb_vcov)

b_kids_gls <- coef(m_gls)["kids"]
se_kids_gls_rb <- sqrt(gls_rb_vcov["kids","kids"])
t_crit_gls <-qt(0.975, df=df.residual(m_gls))
ci_kids_gls_rb <- c(b_kids_gls - t_crit_gls*se_kids_gls_rb,
                    b_kids_gls + t_crit_gls*se_kids_gls_rb)
ci_kids_gls_trad <-confint(m_gls,"kids", level = 0.95)

ci_kids_gls_trad
ci_kids_gls_rb
