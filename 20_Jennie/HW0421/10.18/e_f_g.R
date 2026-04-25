load("D:/碩一下/計量經濟/作業/HW0421/10.18/mroz.rdata")
work$MOTHERCOLL=ifelse(work$mothereduc> 12, 1, 0)
work$FATHERCOLL=ifelse(work$fathereduc> 12, 1, 0)

#d
library(AER)

model=ivreg(log(wage) ~ educ + exper + I(exper^2) |
                   MOTHERCOLL + FATHERCOLL + exper + I(exper^2),
                 data = work)
summary(model)$coefficients

z=qnorm(0.975)
b=coef(model)["educ"]
se=summary(model)$coefficients["educ","Std. Error"]
lower=b - z * se
upper= b + z * se
lower
upper

#e
first_stage=lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = work)
summary(first_stage)

library(car)
linearHypothesis(first_stage,
                 c("MOTHERCOLL = 0",
                   "FATHERCOLL = 0"))
#f
library(AER)
model=ivreg(log(wage) ~ educ + exper + I(exper^2) |
                   MOTHERCOLL + FATHERCOLL + exper + I(exper^2),
                 data = work)

summary(model, diagnostics = TRUE)
