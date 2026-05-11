# Chan Nok Hang 414707007
#hw7 ch11-Q28

library(POE5Rdata)
data("truffles")

library(AER)

# Part B
# 1. Estimate Inverse Demand
# P depends on Q, PS, DI. 
# Q is instrumented by all exogenous variables in the system (PS, DI, PF).
inv_demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(inv_demand_2sls)

# 2. Estimate Inverse Supply
# P depends on Q, PF. 
# Q is instrumented by all exogenous variables in the system (PS, DI, PF).
inv_supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(inv_supply_2sls)

# Part C
# price elasticity of demand
elasticity <- (1/coef(inv_demand_2sls)['q']) * (mean(truffles$p)/mean(truffles$q))
elasticity

# Part F
# 1. Estimate Inverse Demand using OLS
ols_demand <- lm(p ~ q + ps + di, data = truffles)
summary(ols_demand)

# 2. Estimate Inverse Supply using OLS
ols_supply <- lm(p ~ q + pf, data = truffles)
summary(ols_supply)
