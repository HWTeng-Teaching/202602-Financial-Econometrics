library(POE5Rdata)
library(ggplot2)
data(truffles)

#b
demand_iv <- ivreg(p ~ q+ps+di | di+pf+ps, data = truffles)
summary(demand_iv)
supply_iv <- ivreg(p ~ q+pf | di+pf+ps, data = truffles)
summary(supply_iv)

#c
delta1 <- coef(demand_iv)["q"]
Pbar <- mean(truffles$p)
Qbar <- mean(truffles$q)
elasticity <- (1 / delta1) * (Pbar / Qbar)
elasticity

#d
DI0 <- 3.5
PS0 <- 22
PF0 <- 23

invDem <- function(q) coef(demand_iv)["(Intercept)"] +
  coef(demand_iv)["q"]  * q +
  coef(demand_iv)["ps"] * PS0 +
  coef(demand_iv)["di"] * DI0

invSup <- function(q) coef(supply_iv)["(Intercept)"] +
  coef(supply_iv)["q"]  * q +
  coef(supply_iv)["pf"] * PF0

curveData <- data.frame(
  q = seq(0, 35, length = 200)
) |> transform(
  Pd = invDem(q),
  Ps = invSup(q)
)

ggplot(curveData, aes(q)) +
  geom_line(aes(y = Pd), size = 1.1, color = "red",  ) +
  geom_line(aes(y = Ps), size = 1.1, color = "blue") +
  labs(x = "Quantity (q)", y = "Price (p)",
       title = "Inverse Demand (red) and Supply (blue)") +
  theme_minimal()

#e
delta0 <- coef(demand_iv)["(Intercept)"] + coef(demand_iv)["ps"]*PS0 + coef(demand_iv)["di"]*DI0
gamma0 <- coef(supply_iv)["(Intercept)"] + coef(supply_iv)["pf"]*PF0
delta1 <- coef(demand_iv)["q"]
gamma1 <- coef(supply_iv)["q"]

q_star <- (delta0 - gamma0) / (gamma1 - delta1)
p_star <- invDem(q_star)

c(p_star = round(p_star, 2), q_star = round(q_star, 2))

#f
demand_ols <- lm(p ~ q+di,data = truffles)
summary(demand_ols)

supply_ols <- lm(p ~ q+pf+ps,data = truffles)
summary(supply_ols)