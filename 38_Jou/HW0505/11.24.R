library(AER)
library(car)  # linearHypothesis()
library(PoEdata)
data("fultonfish")

# (a)
rf_price <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary(rf_price)

# H0: stormy = 0 and mixed = 0
linearHypothesis(rf_price, c("stormy = 0", "mixed = 0"))

# (b) IVs: stormy, mixed
demand_iv <- ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy + mixed,
  data = fultonfish)

summary(demand_iv)

# (c) Sargan test
summary(demand_iv, diagnostics = TRUE)

# (d)
# Test joint significance of weekday indicators
# H0: mon = tue = wed = thu = 0
linearHypothesis(rf_price,  c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
