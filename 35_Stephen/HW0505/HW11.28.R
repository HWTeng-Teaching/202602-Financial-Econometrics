rm(list=ls())
#install.packages("AER")
library(POE5Rdata)
library(AER)
library(car)
data = truffles

#(b)

#demand
dem_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
coef(summary(dem_iv))

#supply
sup_iv <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
coef(summary(sup_iv))

#(c)

Pbar <- mean(truffles$p)
Qbar <- mean(truffles$q)
elas <- (1 / coef(dem_iv)["q"]) * (Pbar / Qbar)
round(c(Pbar = Pbar, Qbar = Qbar, elasticity = elas), 4)

#(d)

PSF <- 22
DIF <- 3.5
PFF <- 23

d_int <- coef(dem_iv)[1] + coef(dem_iv)["ps"] * PSF + coef(dem_iv)["di"] * DIF
s_int <- coef(sup_iv)[1] + coef(sup_iv)["pf"] * PFF

round(c(
  D_intercept = unname(d_int),
  D_slope_Q   = unname(coef(dem_iv)["q"]),
  S_intercept = unname(s_int),
  S_slope_Q   = unname(coef(sup_iv)["q"])
), 4)

#畫圖
Qgrid <- seq(0, 35, length.out = 200)
plot(
  Qgrid, d_int + coef(dem_iv)["q"] * Qgrid,
  type = "l",
  xlab = "Q",
  ylab = "P",
  ylim = c(0, 120),
  main = "Truffle Demand and Supply"
)
lines(
  Qgrid, s_int + coef(sup_iv)["q"] * Qgrid,
  lty = 2
)
legend(
  "topright",
  legend = c("Demand", "Supply"),
  lty = c(1, 2),
  bty = "n"
)

#(e)

Q_eq <- (d_int - s_int) / (coef(sup_iv)["q"] - coef(dem_iv)["q"])
P_eq <- d_int + coef(dem_iv)["q"] * Q_eq
round(c(P_eq = unname(P_eq), Q_eq = unname(Q_eq)), 4)

rf_q <- lm(q ~ ps + di + pf, data = truffles)
rf_p <- lm(p ~ ps + di + pf, data = truffles)
round(coef(summary(rf_q)), 4)
round(coef(summary(rf_p)), 4)

newdata <- data.frame(ps = 22, di = 3.5, pf = 23)
Q_rf <- predict(rf_q, newdata = newdata)
P_rf <- predict(rf_p, newdata = newdata)
round(c(Q_rf = unname(Q_rf), P_rf = unname(P_rf)), 4)

#compare
round(rbind(
  structural_2SLS = c(Q = unname(Q_eq), P = unname(P_eq)),
  reduced_form    = c(Q = unname(Q_rf), P = unname(P_rf)),
  difference      = c(Q = unname(Q_eq - Q_rf), P = unname(P_eq - P_rf))
), 4)

#(f)

sup_ols <- lm(p ~ q + pf, data = truffles)
round(coef(summary(sup_ols)), 4)

dem_ols <- lm(p ~ q + ps + di, data = truffles)
round(coef(summary(dem_ols)), 4)
