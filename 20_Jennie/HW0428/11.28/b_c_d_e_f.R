load("D:/碩一下/計量經濟/作業/HW0428/11.28/truffles.rdata")
names(truffles)

#b
library(AER)

demand_iv =ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_iv)

supply_iv =ivreg(p ~ q + pf | pf + ps + di, data = truffles)
summary(supply_iv)

#c
mean_p=mean(truffles$p)
mean_q=mean(truffles$q)

elasticity=(1 / -2.671) * (mean_p / mean_q)
elasticity

#d
Q=seq(0, 50, by = 0.1)
DI=3.5
PS=22
PF=23

P_d=-11.428 - 2.671*Q + 3.461*PS + 13.390*DI
P_s=-58.7982 + 2.9367*Q + 2.9585*PF

plot(Q, P_d, type = "l", lwd = 2,
     col = "blue",
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Supply and Demand for Truffles")

lines(Q, P_s, lwd = 2, col = "red")

legend("topright",
       legend = c("Demand", "Supply"),
       col = c("blue", "red"),
       lwd = 2)

#e
Q_star = (111.579 - 9.2473) / (2.9367 + 2.671)
P_star = 111.579 - 2.671 * Q_star
Q_star
P_star

Q_hat= 7.8951 + 0.6564*PS + 2.1672*DI - 0.5070*PF
P_hat=-32.5124 + 1.7081*PS + 7.6025*DI + 1.3539*PF
Q_hat
P_hat

#f
demand_ols=lm(p ~ q + ps + di, data = truffles)
summary(demand_ols)

supply_ols=lm(p ~ q + pf, data = truffles)
summary(supply_ols)
