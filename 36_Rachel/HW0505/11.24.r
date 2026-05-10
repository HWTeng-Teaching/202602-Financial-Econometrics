if(!require(AER)) install.packages("AER")
if(!require(PoEdata)) install.packages("PoEdata") 
library(AER)
library(PoEdata)
library(car)
data("fultonfish")

#a
model1 <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary_model1 <- summary(model1)
print(summary_model1)
joint_test_weather <- linearHypothesis(model1, c("stormy = 0", "mixed = 0"))
print(joint_test_weather)

#b
demand_2sls <- ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary(demand_2sls)

#c
summary(demand_2sls, diagnostics = TRUE)

#d
joint_test_days <- linearHypothesis(model1, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
print(joint_test_days)
