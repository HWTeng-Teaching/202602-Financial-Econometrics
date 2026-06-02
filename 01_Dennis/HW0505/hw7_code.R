# Homework 7 - Chapter 11
# Questions 11.24 and 11.28

library(POE5Rdata)
library(AER)
library(sandwich)
library(lmtest)

# ---------------------------------------------------------
# Question 11.24 - Fulton Fish Market
# ---------------------------------------------------------
cat("--- Question 11.24 ---\n")
data("fultonfish")

# a. Estimate new reduced-form for ln(PRICE)
# Original exogenous: mon, tue, wed, thu, stormy
# New exogenous: mixed
rf_a <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
cat("10.24a: Reduced form for lprice\n")
print(summary(rf_a))

# Test significance of MIXED
cat("\nTest significance of MIXED:\n")
print(linearHypothesis(rf_a, "mixed = 0"))

# Test joint significance of STORMY and MIXED
cat("\nTest joint significance of STORMY and MIXED:\n")
lh_joint <- linearHypothesis(rf_a, c("stormy = 0", "mixed = 0"))
print(lh_joint)
cat("F-value: ", lh_joint$F[2], "\n\n")

# b. Estimate demand equation using STORMY and MIXED as IVs
# Demand: lquan ~ lprice + mon + tue + wed + thu
iv_demand_b <- ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy + mixed, data = fultonfish)
cat("10.24b: 2SLS Estimates for Fish Demand\n")
print(summary(iv_demand_b))

# c. Sargan test for the surplus instrument
cat("10.24c: Diagnostic tests (including Sargan)\n")
print(summary(iv_demand_b, diagnostics = TRUE))

# d. Test joint significance of MON, TUE, WED, THU in reduced form part (a)
cat("10.24d: Test joint significance of daily indicators in reduced form\n")
print(linearHypothesis(rf_a, c("mon = 0", "tue = 0", "wed = 0", "thu = 0")))


# ---------------------------------------------------------
# Question 11.28 - Truffle Data
# ---------------------------------------------------------
cat("\n--- Question 11.28 ---\n")
data("truffles")

# a. Rewrite equations with price P on LHS
# Demand: Q = a1 + a2*P + a3*PS + a4*DI  => P = (-a1/a2) + (1/a2)*Q - (a3/a2)*PS - (a4/a2)*DI
# Supply: Q = b1 + b2*P + b3*PF          => P = (-b1/b2) + (1/b2)*Q - (b3/b2)*PF

# b. Estimate using 2SLS
# In both cases, the instrument set is the full set of exogenous variables: PS, DI, PF
iv_demand_p <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
iv_supply_p  <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)

cat("10.28b: 2SLS Rewritten Demand (P on LHS)\n")
print(summary(iv_demand_p))
cat("\n10.28b: 2SLS Rewritten Supply (P on LHS)\n")
print(summary(iv_supply_p))

# c. Price elasticity of demand "at the means"
# Elasticity = (dQ/dP) * (P/Q)
# From rewritten demand: P = g1 + g2*Q + g3*PS + g4*DI
# dP/dQ = g2  => dQ/dP = 1/g2
# So Elasticity = (1/g2) * (mean(P) / mean(Q))
g2 <- coef(iv_demand_p)["q"]
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)
elasticity <- (1/g2) * (mean_p / mean_q)
cat("\n10.28c: Price elasticity of demand at the means\n")
cat("Mean P: ", mean_p, ", Mean Q: ", mean_q, "\n")
cat("g2 (dP/dQ): ", g2, "\n")
cat("Elasticity: ", elasticity, "\n")

# d. Sketch equations with P on vertical axis, Q on horizontal
# DI = 3.5, PF = 23, PS = 22
# Demand: P = g1 + g2*Q + g3*22 + g4*3.5
# Supply: P = d1 + d2*Q + d3*23
# We'll calculate the intercepts for the P vs Q plot
c_d <- coef(iv_demand_p)
demand_intercept <- c_d[1] + c_d["ps"]*22 + c_d["di"]*3.5
demand_slope <- c_d["q"]

c_s <- coef(iv_supply_p)
supply_intercept <- c_s[1] + c_s["pf"]*23
supply_slope <- c_s["q"]

cat("\n10.28d: Intercepts and slopes for P vs Q plot (DI=3.5, PF=23, PS=22)\n")
cat("Demand: P = ", demand_intercept, " + (", demand_slope, ")*Q\n")
cat("Supply: P = ", supply_intercept, " + (", supply_slope, ")*Q\n")

# e. Equilibrium values from part (d)
# demand_intercept + demand_slope * Q = supply_intercept + supply_slope * Q
# Q * (demand_slope - supply_slope) = supply_intercept - demand_intercept
q_eq <- (supply_intercept - demand_intercept) / (demand_slope - supply_slope)
p_eq <- demand_intercept + demand_slope * q_eq
cat("\n10.28e: Equilibrium values\n")
cat("Q_eq: ", q_eq, ", P_eq: ", p_eq, "\n")

# Predicted equilibrium values using reduced form equations from Table 11.2
# Let's estimate them first
rf_q <- lm(q ~ ps + di + pf, data = truffles)
rf_p <- lm(p ~ ps + di + pf, data = truffles)
q_pred <- predict(rf_q, newdata = data.frame(ps=22, di=3.5, pf=23))
p_pred <- predict(rf_p, newdata = data.frame(ps=22, di=3.5, pf=23))
cat("Predicted from reduced-form:\n")
cat("Q_pred: ", q_pred, ", P_pred: ", p_pred, "\n")

# f. Estimate using OLS
ols_demand_p <- lm(p ~ q + ps + di, data = truffles)
ols_supply_p  <- lm(p ~ q + pf, data = truffles)
cat("\n10.28f: OLS Rewritten Demand\n")
print(summary(ols_demand_p))
cat("\n10.28f: OLS Rewritten Supply\n")
print(summary(ols_supply_p))
