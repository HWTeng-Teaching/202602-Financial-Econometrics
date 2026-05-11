library(AER)
library(car)
library(PoEdata)
data(truffles)

# (b) 
# Demand
demand_2sls <- ivreg(p ~ q + ps + di | pf + ps + di, data = truffles)
summary(demand_2sls)

# Supply
supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(supply_2sls)

# (c) Demand elasticity at the means
delta2 <- coef(demand_2sls)["q"]

p_bar <- mean(truffles$p)
q_bar <- mean(truffles$q)

elasticity_demand <- (1 / delta2) * (p_bar / q_bar)

elasticity_demand

# (d) Sketch demand and supply curves
di_star <- 3.5
pf_star <- 23
ps_star <- 22

d <- coef(demand_2sls)
g <- coef(supply_2sls)

# 建立 Q 的範圍
q_grid <- seq(0, 35, length.out = 100)

# Demand price
p_demand <- d["(Intercept)"] + d["q"]  * q_grid + d["ps"] * ps_star + d["di"] * di_star

# Supply price
p_supply <- g["(Intercept)"] + g["q"]  * q_grid + g["pf"] * pf_star

# Plot
plot(q_grid, p_demand,type = "l",
  ylim = range(c(p_demand, p_supply)), xlab = "Quantity (Q)", ylab = "Price (P)",
  main = "Truffle Demand and Supply Curves")

lines(q_grid, p_supply, lty = 2)

legend("topright", legend = c("Demand", "Supply"), lty = c(1, 2), bty = "n")

# (e) Equilibrium P and Q
demand_intercept <- d["(Intercept)"] + d["ps"] * ps_star + d["di"] * di_star
demand_slope <- d["q"]

supply_intercept <- g["(Intercept)"] + g["pf"] * pf_star
supply_slope <- g["q"]

q_eq <- (supply_intercept - demand_intercept) / (demand_slope - supply_slope)
p_eq <- demand_intercept + demand_slope * q_eq

q_eq
p_eq

# Reduced-form equations
rf_q <- lm(q ~ ps + di + pf, data = truffles)
rf_p <- lm(p ~ ps + di + pf, data = truffles)

summary(rf_q)
summary(rf_p)

newdata <- data.frame(ps = ps_star, di = di_star, pf = pf_star)

q_eq_rf <- predict(rf_q, newdata = newdata)
p_eq_rf <- predict(rf_p, newdata = newdata)

q_eq_rf
p_eq_rf

# (f) OLS estimation
demand_ols <- lm(p ~ q + ps + di, data = truffles)

supply_ols <- lm(p ~ q + pf,  data = truffles)

summary(demand_ols)
summary(supply_ols)
