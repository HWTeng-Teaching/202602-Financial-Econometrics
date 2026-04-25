load("D:/碩一下/計量經濟/作業/HW0421/10.20/capm5.rdata")
capm5$msft_excess=capm5$msft - capm5$riskfree
capm5$mkt_excess=capm5$mkt  - capm5$riskfree
capm5$RANK=rank(capm5$mkt_excess, ties.method = "first")

#e
capm5$POS=ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage=lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage)

#f
capm5$vhat2=resid(first_stage)

hausman_model= lm(msft_excess ~ mkt_excess + vhat2, data = capm5)
summary(hausman_model)

#g
library(AER)

iv_final=ivreg(msft_excess ~ mkt_excess |
                    RANK + POS,
                  data = capm5)

summary(iv_final)

#h
iv_model=ivreg(msft_excess ~ mkt_excess |
                 RANK,
               data = capm5)
uhat=resid(iv_model)

sargan_test=lm(uhat ~ RANK + POS, data = capm5)
summary(sargan_test)

n=nrow(capm5)
R2= summary(sargan_test)$r.squared
S= n * R2
S

qchisq(0.95, df = 1)
