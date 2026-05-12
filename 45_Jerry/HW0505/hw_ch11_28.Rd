library(POE5Rdata)
library(AER)   # ivreg() for 2SLS
library(car)   # linearHypothesis()

data(truffles)

# ============================================================
# Part (b)
# ============================================================
inv_demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf,
                         data = truffles)

inv_supply_2sls <- ivreg(p ~ q + pf | pf + ps + di,
                         data = truffles)

print(summary(inv_demand_2sls))

print(summary(inv_supply_2sls))

# ============================================================
# Part (c)
# ============================================================

d_coef <- coef(inv_demand_2sls)
s_coef <- coef(inv_supply_2sls)
a2_hat    <- d_coef["q"]
alpha2    <- 1 / a2_hat
p_bar     <- mean(truffles$p)
q_bar     <- mean(truffles$q)
elasticity <- alpha2 * (p_bar / q_bar)
print(elasticity)


# ============================================================
# Part (d)
# ============================================================

DI_star <- 3.5
PF_star <- 23
PS_star <- 22

intercept_d <- d_coef["(Intercept)"] + d_coef["ps"] * PS_star + d_coef["di"] * DI_star
slope_d     <- d_coef["q"]
intercept_s <- s_coef["(Intercept)"] + s_coef["pf"] * PF_star
slope_s     <- s_coef["q"]

cat(sprintf("Inverse Demand（PS*=22, DI*=3.5）: P = %.4f + %.4f * Q\n",
            intercept_d, slope_d))
cat(sprintf("Inverse Supply（PF*=23）:           P = %.4f + %.4f * Q\n\n",
            intercept_s, slope_s))

Q_range <- seq(0, max(truffles$q) * 1.5, length.out = 200)
P_demand <- intercept_d + slope_d * Q_range
P_supply <- intercept_s + slope_s * Q_range

plot(Q_range, P_demand,
     type = "l", col = "blue", lwd = 2,
     xlim = c(0, max(Q_range)),
     ylim = c(0, max(c(P_demand, P_supply), na.rm = TRUE) * 1.1),
     xlab = "Quantity (Q, oz)", ylab = "Price (P, $/oz)",
     main = "Truffle Market: Inverse Supply & Demand\n(DI*=3.5, PS*=22, PF*=23)")
lines(Q_range, P_supply, col = "red", lwd = 2)
legend("topright",
       legend = c("Inverse Demand", "Inverse Supply"),
       col = c("blue", "red"), lwd = 2)


# ============================================================
# Part (e)
# ============================================================

Q_eq <- (intercept_s - intercept_d) / (slope_d - slope_s)
P_eq <- intercept_d + slope_d * Q_eq

Q_rf_book <- 7.8951 + 0.6564 * PS_star + 2.1672 * DI_star - 0.5070 * PF_star
P_rf_book <- -32.5124 + 1.7081 * PS_star + 7.6025 * DI_star + 1.3539 * PF_star

comparison <- data.frame(
  Method   = c("2SLS", "Table 11.2"),
  Q_hat    = round(c(Q_eq, Q_rf_book), 4),
  P_hat    = round(c(P_eq, P_rf_book), 4)
)
print(comparison)
 
# ============================================================
# Part (f)
# ============================================================

inv_demand_ols <- lm(p ~ q + ps + di, data = truffles)
print(summary(inv_demand_ols))
 
inv_supply_ols <- lm(p ~ q + pf,      data = truffles)
print(summary(inv_supply_ols))

d_ols <- coef(inv_demand_ols)
d_2sl <- coef(inv_demand_2sls)
demand_tbl <- data.frame(
  Variable = c("Intercept", "Q", "PS", "DI"),
  OLS      = round(d_ols, 4),
  `2SLS`   = round(d_2sl, 4),
  Diff     = round(d_2sl - d_ols, 4),
  row.names = NULL
)
print(demand_tbl)

s_ols <- coef(inv_supply_ols)
s_2sl <- coef(inv_supply_2sls)
supply_tbl <- data.frame(
  Variable = c("Intercept", "Q", "PF"),
  OLS      = round(s_ols, 4),
  `2SLS`   = round(s_2sl, 4),
  Diff     = round(s_2sl - s_ols, 4),
  row.names = NULL
)
print(supply_tbl)

 