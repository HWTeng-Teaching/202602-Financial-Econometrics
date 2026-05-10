library(PoEdata)
library(AER)
data(truffles)
#b小題
model_demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
model_supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
cat("--- 2SLS 需求方程結果 ---\n")
summary(model_demand_2sls)
cat("\n--- 2SLS 供給方程結果 ---\n")
summary(model_supply_2sls)

#c小題
gamma2<-coef(model_demand_2sls)["q"]
p_mean<-mean(truffles$p)
q_mean<-mean(truffles$q)
elasticity<-(1/gamma2)*p_mean/q_mean
cat("平均值處的需求價格彈性為:", elasticity, "\n")

#e小題
q_star <- 7.8951 + 0.6564*(22) + 2.1672*(3.5) - 0.5070*(23)
p_star <- -32.5124 + 1.7081*(22) + 7.6025*(3.5) + 1.3539*(23)
cat("預測均衡價格 P*:", p_star, " 均衡數量 Q*:", q_star, "\n")

#f小題
model_demand_ols<-lm(p~q+ps+di,data=truffles)
model_supply_ols <- lm(p ~ q + pf, data = truffles)
summary(model_demand_ols)
summary(model_supply_ols)