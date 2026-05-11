#11.24
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/fultonfish.rdata"))
library(AER)
#a
reduced <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary(reduced)

#join test
linearHypothesis(reduced,c("stormy=0","mixed=0"))

#b
demand_iv <- ivreg(lquan~ lprice + mon + tue + wed + thu |
                          mon + tue + wed + thu + stormy + mixed,data=fultonfish)
summary (demand_iv)


#c
summary(demand_iv, diagnostics = TRUE)

#d
#join test
linearHypothesis(reduced,c("mon=0","tue=0","wed=0","thu=0"))

#11.28
#b
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"))
sup_iv <-ivreg(p~q + pf|
                 ps + di + pf,data=truffles)
summary(sup_iv)
dem_iv <-ivreg(p~q + ps + di|
                 ps + di + pf,data=truffles)
summary(dem_iv)

#c
a2 <- coef(dem_iv)

p_mean <- mean(truffles$p)
q_mean <- mean(truffles$q)
E <- (1/a2["q"])*(p_mean/q_mean)

#d
DI0 <- 3.5
PF0 <- 23
PS0 <- 22

b2 <- coef(sup_iv)

# Demand line
demand_intercept <- a2["(Intercept)"] + a2["ps"] * PS0 + a2["di"] * DI0
demand_slope <- a2["q"]

# Supply line
supply_intercept <- b2["(Intercept)"] + b2["pf"] * PF0
supply_slope <- b2["q"]

#e
e_q <-(demand_intercept-supply_intercept)/(supply_slope-demand_slope)
e_p <- demand_intercept + demand_slope*e_q
e_q
e_p

#predict
rf_q <-lm(q ~ ps + di + pf, data = truffles)
rf_p <-lm(p ~ ps + di + pf, data = truffles)

pred <- data.frame(ps=PS0, di=DI0, pf=PF0)

pred_q <- predict(rf_q,newdata = pred )
pred_p <- predict(rf_p,newdata = pred )
pred_q
pred_p

#f
sup_ols <- lm(p ~ q + pf, data=truffles)
summary(sup_ols)

dem_ols <- lm(p ~ q + ps + di, data=truffles )
summary(dem_ols)
