load("D:/碩一下/計量經濟/作業/HW0421/10.18/mroz.rdata")

#a
work=subset(mroz, lfp == 1)
nrow(work)

work$MOTHERCOLL=ifelse(work$mothereduc> 12, 1, 0)
work$FATHERCOLL=ifelse(work$fathereduc> 12, 1, 0)

mean(work$MOTHERCOLL, na.rm = TRUE) * 100
mean(work$FATHERCOLL, na.rm = TRUE) * 100

#b
cor(work[, c("educ","MOTHERCOLL","FATHERCOLL")], use="complete.obs")

#c
library(AER)
model=ivreg(log(wage) ~ educ + exper + I(exper^2) |
                    MOTHERCOLL + exper + I(exper^2),
                  data = work)
summary(model)$coefficients

z=qnorm(0.975)
b=coef(model)["educ"]
se=summary(model)$coefficients["educ","Std. Error"]
lower = b - z * se
upper = b + z * se
lower
upper

#d
first_stage=lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = work)
summary(first_stage)

library(car)
linearHypothesis(first_stage, "MOTHERCOLL = 0")
