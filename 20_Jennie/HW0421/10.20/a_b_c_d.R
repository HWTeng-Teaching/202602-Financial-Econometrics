load("D:/碩一下/計量經濟/作業/HW0421/10.20/capm5.rdata")
names(capm5)

#a
capm5$msft_excess=capm5$msft - capm5$riskfree
capm5$mkt_excess=capm5$mkt  - capm5$riskfree

model=lm(msft_excess ~ mkt_excess, data = capm5)

summary(model)

#b
capm5$RANK=rank(capm5$mkt_excess, ties.method = "first")
first_stage=lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)

#c
capm5$vhat=resid(first_stage)
model2=lm(msft_excess ~ mkt_excess + vhat, data = capm5)
summary(model2)

#d
library(AER)

iv_model=ivreg(msft_excess ~ mkt_excess |
                    RANK,
                  data = capm5)

summary(iv_model)
