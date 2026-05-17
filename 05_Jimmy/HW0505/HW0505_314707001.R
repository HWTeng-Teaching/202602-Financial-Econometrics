rm(list=ls())

library(POE5Rdata)
library(AER) 
library(car)

data('fultonfish')

# A.

rf_price = lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)

summary(rf_price) 

linearHypothesis(rf_price, c("stormy = 0", "mixed = 0"))

# B.
demand_iv = ivreg(lquan ~ lprice + mon + tue + wed + thu | 
                     mon + tue + wed + thu + stormy + mixed, 
                   data = fultonfish)

summary(demand_iv)

# C.
summary(demand_iv, diagnostics = TRUE)

# D.

linearHypothesis(rf_price, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))


# 11.28

data('truffles')

# a.
# b.
inv_demand = ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(inv_demand)

inv_supply = ivreg(p ~ q + pf | pf + ps + di, data = truffles)
summary(inv_supply)

# c.
gamma_2 = coef(inv_demand)["q"]
mean_P = 62.72
mean_Q = 18.46
elasticity = (1 / gamma_2) * (mean_P / mean_Q)
print(paste("Price Elasticity of Demand at the means is:", round(elasticity, 4)))

# d.

val_PS = 22
val_DI = 3.5
val_PF = 23

d_coef = coef(inv_demand)
new_intercept_D = d_coef["(Intercept)"] + d_coef["ps"] * val_PS + d_coef["di"] * val_DI
slope_D = d_coef["q"]

s_coef = coef(inv_supply)
new_intercept_S = s_coef["(Intercept)"] + s_coef["pf"] * val_PF
slope_S = s_coef["q"]


cat("繪圖用需求方程式: P =", round(new_intercept_D, 2), "+ (", round(slope_D, 2), ") * Q \n")
cat("繪圖用供給方程式: P =", round(new_intercept_S, 2), "+ (", round(slope_S, 2), ") * Q \n")


plot(0, 0, type = "n", xlim = c(0, 40), ylim = c(0, 120),
     xlab = "Quantity (Q)", ylab = "Price (P)", 
     main = "Truffle Supply and Demand")
abline(a = new_intercept_D, b = slope_D, col = "red", lwd = 2)
abline(a = new_intercept_S, b = slope_S, col = "blue", lwd = 2)
legend("topright", legend = c("Demand", "Supply"), 
       col = c("red", "blue"), lwd = 2)
# e. 
PS_val = 22
DI_val = 3.5
PF_val = 23

Q_reduced = 7.8951 + 0.6564 * PS_val + 2.1672 * DI_val - 0.5070 * PF_val
P_reduced = -32.5124 + 1.7081 * PS_val + 7.6025 * DI_val + 1.3539 * PF_val

cat("Reduced-form 預測均衡數量 Q* =", Q_reduced, "\n")
cat("Reduced-form 預測均衡價格 P* =", P_reduced, "\n\n")


int_D = new_intercept_D
int_S = new_intercept_S

Q = (int_D - int_S) / (slope_S - slope_D)

Q_structural = (int_D - int_S) / (slope_S - slope_D)
P_structural = int_D + slope_D * Q_structural

cat("Structural (Part d) 預測均衡數量 Q* =", Q_structural, "\n")
cat("Structural (Part d) 預測均衡價格 P* =", P_structural, "\n")

# f.

ols_demand = lm(p ~ q + ps + di, data = truffles)
summary(ols_demand)

ols_supply = lm(p ~ q + pf, data = truffles)
summary(ols_supply)




