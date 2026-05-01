library(POE5Rdata)
library(AER)
data(capm5)

#a
capm5$msft_excess = capm5$msft-capm5$riskfree
capm5$mkt_excess = capm5$mkt-capm5$riskfree
model_ols=lm(msft_excess~mkt_excess, data=capm5)
summary(model_ols)

#b
#由小到大排序
capm5$RANK <- rank(capm5$mkt_excess, ties.method = "first")
first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)

#c
capm5$v_hat <- residuals(first_stage)
model_augmented <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(model_augmented)

#d
model_iv <- ivreg(msft_excess ~ mkt_excess | RANK, data = capm5)
summary(model_iv)

#e
capm5$POS <- ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage2 <- lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage2)

#f
first_stage3 <- lm(mkt_excess ~ RANK + POS, data = capm5)
capm5$v_hat2 <- residuals(first_stage3)
model_hausman <- lm(msft_excess ~ mkt_excess + v_hat2, data = capm5)
summary(model_hausman)

#g
model_iv2 <- ivreg(msft_excess ~ mkt_excess | RANK + POS, data = capm5)
summary(model_iv2)

#h
model_iv3=ivreg(msft_excess ~ mkt_excess | RANK, data = capm5)
uhat=resid(model_iv3)
sargan_test=lm(uhat ~ RANK + POS, data = capm5)
summary(sargan_test)

n=nrow(capm5)
R2= summary(sargan_test)$r.squared
S= n*R2
S
qchisq(0.95, df = 1)