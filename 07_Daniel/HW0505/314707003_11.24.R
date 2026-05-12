rm(list=ls())
#install.packages("AER")
library(POE5Rdata)
library(AER)
library(car)
data = fultonfish

#11.24(a)
rf_price <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
# check p-value of mixed
coef(summary(rf_price))
# check the joint significance of STORMY and MIXED
linearHypothesis(rf_price, c("stormy = 0", "mixed = 0"))

#11.24(b)
demand_iv <- ivreg(lquan ~ lprice + mon + tue + wed + thu |
    mon + tue + wed + thu + stormy + mixed,data = fultonfish)
coef(summary(demand_iv))

#11.24(c)
u_hat <- resid(demand_iv)
sargan_aux <- lm(u_hat ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
S <- nobs(demand_iv) * summary(sargan_aux)$r.squared
p_value <- pchisq(S, df = 1, lower.tail = FALSE)
round(c(Sargan = S, df = 1, p.value = p_value), 4)

#11.24(d)
linearHypothesis(rf_price, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
