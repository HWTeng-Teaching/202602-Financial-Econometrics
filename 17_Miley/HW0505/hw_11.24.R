load("Documents/R/data_needed/poe5rdata/fultonfish.rdata")
#------------------------------------------------------------------------------
# part a

# Add the variable MIXED which indicating poor but not STORMY weather condition
# OLS: 把內生的 ln(PRICE) 拿去對所有的外生變數跑迴歸
reduced_form <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)

# Print the result
summary(reduced_form)

# Test the joint significance of STORMY and MIXED
linearHypothesis(reduced_form, c("stormy = 0", "mixed = 0"))
#------------------------------------------------------------------------------
# part b

# Run 2SLS model
demand_iv_model <- ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary(demand_iv_model)
#------------------------------------------------------------------------------
# part c

# Sargan test
summary(demand_iv_model, diagnostics = TRUE)
#------------------------------------------------------------------------------
# part d

# Test the joint significance of MON, TUE, WED, THU
linearHypothesis(reduced_form, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))


