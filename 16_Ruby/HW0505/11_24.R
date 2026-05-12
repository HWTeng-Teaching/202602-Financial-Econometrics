library(POE5Rdata)
library(AER)
library(car)
library(lmtest)
library(sandwich)
data(fultonfish)

fultonfish$MIXED <- fultonfish$mixed

#a
rf_model <- lm(lprice ~ stormy + MIXED + mon + tue + wed + thu, data = fultonfish)
summary(rf_model)
summary(rf_model)$coefficients["MIXED", ]
joint_test <- linearHypothesis(rf_model, c("stormy = 0","MIXED = 0"))
joint_test
F_value <- joint_test$F[2]
F_value

#b
iv_model <- ivreg(lquan ~ lprice + mon + tue + wed + thu |stormy + MIXED + mon + tue + wed + thu, data = fultonfish)
summary(iv_model)

#c
iv_model_diag <- summary(iv_model,diagnostics = TRUE)
iv_model_diag$diagnostics
sargan_test <- iv_model_diag$diagnostics["Sargan", ]
sargan_test

#d
weekday_test <- linearHypothesis(rf_model, c("mon = 0","tue = 0","wed = 0","thu = 0"))
weekday_test