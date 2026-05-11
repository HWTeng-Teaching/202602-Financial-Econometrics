# Chan Nok Hang 414707007
#hw7 ch11-Q24

library(POE5Rdata)
data("fultonfish")

library(car)
library(AER)

# Part A
# Estimate the reduced-form OLS regression
reduced_form_p <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)

# Output the summary to check the significance of MIXED
summary(reduced_form_p)

# Test joint significance of STORMY and MIXED (Excluded Instruments for Demand)
# H0: pi_62 = 0, pi_72 = 0
joint_test_weather <- linearHypothesis(reduced_form_p, c("stormy = 0", "mixed = 0"))
print(joint_test_weather)

# Part B
# Estimate the structural demand equation using 2SLS
# Formula syntax: y ~ endogenous + included_exogenous | included_exogenous + excluded_exogenous
demand_2sls <- ivreg(lquan ~ lprice + mon + tue + wed + thu | 
                       mon + tue + wed + thu + stormy + mixed, 
                     data = fultonfish)

# View the results
summary(demand_2sls)

# Part C
# Request diagnostics to get the Sargan test automatically
summary(demand_2sls, diagnostics = TRUE)

# Part D
# Test joint significance of the days of the week in the reduced-form equation
# H0: pi_22 = 0, pi_32 = 0, pi_42 = 0, pi_52 = 0
joint_test_days <- linearHypothesis(reduced_form_p, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
print(joint_test_days)
