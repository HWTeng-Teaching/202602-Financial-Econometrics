rm(list=ls())
library(PoEdata)
data(lasvegas)

library(sandwich)
library(lmtest)

#a.
lpm_model = lm(
  delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
  data = lasvegas
)

summary(lpm_model)

coeftest(lpm_model, vcov = vcovHC(lpm_model, type = "HC1"))

#b.

logit_model = glm(
  delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
  data = lasvegas,
  family = binomial(link = "logit")
)

summary(logit_model)

#c.

obs_500_1000 = lasvegas[c(500, 1000), ]


lpm_pred = predict(
  lpm_model,
  newdata = obs_500_1000,
  type = "response"
)


logit_pred = predict(
  logit_model,
  newdata = obs_500_1000,
  type = "response"
)


pred_results = data.frame(
  observation = c(500, 1000),
  actual_delinquent = obs_500_1000$delinquent,
  lpm_predicted_probability = lpm_pred,
  logit_predicted_probability = logit_pred
)

pred_results

#d.
