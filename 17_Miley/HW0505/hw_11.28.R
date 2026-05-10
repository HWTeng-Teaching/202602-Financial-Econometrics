load("Documents/R/data_needed/poe5rdata/truffles.rdata")

# ----------------------------------------------------------
# part b

# 把兩條方程式寫成一個 list
truffle_D <- p ~ q + ps + di
truffle_S <- p ~ q + pf
truffle_eqs <- list(truffle_D, truffle_S)

# 把所有外生變數寫在一起
truffle_ivs <- ~ ps + di + pf

# 使用 systemfit 並指定 method = "2SLS"
truffle_sys <- systemfit(truffle_eqs, method = "2SLS", inst = truffle_ivs, data = truffles)

# Print the result
summary(truffle_sys)
# ----------------------------------------------------------
# part c

# Retrieve the mean of P and Q
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)

# Retrieve the coefficient of truffle_D
gamma1 <- coef(truffle_sys)["eq1_q"]

# Calculate the elasticity E = dQ/dP * P/Q
elasticity <- (1/gamma1)*(mean_p/mean_q)

# Print the result
cat("The price elasticity of demand at the means is :", elasticity, "\n")
# ----------------------------------------------------------
# part d

# 載入畫圖套件
library(ggplot2)

# Retrieve the coefficient
coef_D <- coef(truffle_sys$eq[[1]])
coef_S <- coef(truffle_sys$eq[[2]])

# Set up the value of DI, PF and PS
di_star <- 3.5
pf_star <- 23
ps_star <- 22

# Calculate the new intercept and slope
# Demand (套用 as.numeric 確保是純數字)
intercept_D <- as.numeric(coef_D["(Intercept)"] + coef_D["ps"]*ps_star + coef_D["di"]*di_star)
slope_D <- as.numeric(coef_D["q"])

# Calculate the new intercept and slope
# Supply (套用 as.numeric 確保是純數字)
intercept_S <- as.numeric(coef_S["(Intercept)"] + coef_S["pf"]*pf_star)
slope_S <- as.numeric(coef_S["q"])

# Sketch the demand and supply curve
truffle_plot <- ggplot(data = truffles, aes(x = q, y = p)) +
  geom_point(color = "gray", alpha = 0.5) +
  geom_abline(intercept = intercept_D, slope = slope_D, color = "lightblue", linewidth = 1.5) +
  geom_abline(intercept = intercept_S, slope = slope_S, color = "pink", linewidth = 1.5) +
  labs(title = "Truffles Market", x = "Quantity", y = "Price") +
  theme_minimal()

# 顯示圖表
print(truffle_plot)
# ----------------------------------------------------------
# part e

# Solve the simultaneous equations: intercept_D + slope_D*Q = intercept_S + slope_S*Q
q_star <- (intercept_S - intercept_D) / (slope_D - slope_S)
p_star <- intercept_D + slope_D * q_star

cat("The equilibium quantity is: ", q_star, "\n")
cat("The equilibium price is: ", p_star, "\n")

# Predicted equibrium values of P and Q
predicted_q_star <- 7.8951 + 0.6564*ps_star + 2.1672*di_star + (-0.5070)*pf_star
predicted_p_star <- -32.5124 + 1.7081*ps_star + 7.6025*di_star + 1.3539*pf_star

cat("The predicted equilibrium quantity is: ", predicted_q_star, "\n")
cat("The predicted equilibrium price is: ", predicted_p_star, "\n")
# ----------------------------------------------------------
# part f

# Estimate the demand equation using OLS
demand_ols <- lm(p ~ q + ps + di, data = truffles)
summary(demand_ols)

# Estimate the supply equation using OLS
supply_ols <- lm(p ~ q + pf, data = truffles)
summary(supply_ols)

