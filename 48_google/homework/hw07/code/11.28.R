# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 11 - Q28
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)
library(AER)   # ivreg (2SLS)
library(car)   # linearHypothesis

data("truffles")
head(truffles)
summary(truffles)

# -------------------------------------------------------
# (a) 預期符號說明（無需程式碼，僅輸出提示）
# -------------------------------------------------------
cat("\n===== (a) 反需求與反供給方程式預期符號 =====\n")
cat("反需求：P = a0 + a2*Q + a3*PS + a4*DI；預期 a2<0, a3>0, a4>0\n")
cat("反供給：P = b0 + b2*Q + b3*PF；       預期 b2>0, b3<0\n")

# -------------------------------------------------------
# (b) 2SLS 估計反需求與反供給方程式
# -------------------------------------------------------

# 反需求：P = f(Q, PS, DI)，內生變數 Q，IV = PF
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf,
                     data = truffles)
cat("\n===== (b) 2SLS 反需求方程式 =====\n")
print(summary(demand_2sls, diagnostics = TRUE))

# 反供給：P = f(Q, PF)，內生變數 Q，IV = PS, DI
supply_2sls <- ivreg(p ~ q + pf | pf + ps + di,
                     data = truffles)
cat("\n===== (b) 2SLS 反供給方程式 =====\n")
print(summary(supply_2sls, diagnostics = TRUE))

# -------------------------------------------------------
# (c) 需求價格彈性（在均值處）
# -------------------------------------------------------
# 反需求：P = a0 + a2*Q + ...  =>  dP/dQ = a2  =>  dQ/dP = 1/a2
# 彈性 = dQ/dP * (P_bar / Q_bar)
a2_hat    <- coef(demand_2sls)["q"]
p_bar     <- mean(truffles$p)
q_bar     <- mean(truffles$q)
elasticity <- (1 / a2_hat) * (p_bar / q_bar)

cat("\n===== (c) 需求價格彈性（在均值處）=====\n")
cat(sprintf("a2_hat (dP/dQ) = %.4f\n", a2_hat))
cat(sprintf("P_bar  = %.4f\n", p_bar))
cat(sprintf("Q_bar  = %.4f\n", q_bar))
cat(sprintf("需求價格彈性 = %.4f\n", elasticity))

# -------------------------------------------------------
# (d) 固定外生變數，畫供需曲線
# -------------------------------------------------------
DI_star <- 3.5
PF_star <- 23
PS_star <- 22

coef_d <- coef(demand_2sls)   # P = a0 + a2*Q + a3*PS + a4*DI
coef_s <- coef(supply_2sls)   # P = b0 + b2*Q + b3*PF

# 固定外生變數後的截距
d_intercept <- coef_d["(Intercept)"] + coef_d["ps"] * PS_star + coef_d["di"] * DI_star
d_slope     <- coef_d["q"]

s_intercept <- coef_s["(Intercept)"] + coef_s["pf"] * PF_star
s_slope     <- coef_s["q"]

cat("\n===== (d) 固定外生變數後的供需方程式 =====\n")
cat(sprintf("反需求：P = %.4f + (%.4f) * Q\n", d_intercept, d_slope))
cat(sprintf("反供給：P = %.4f + (%.4f) * Q\n", s_intercept, s_slope))

# 畫圖
q_range  <- seq(0, 30, length.out = 300)
p_demand <- d_intercept + d_slope * q_range
p_supply <- s_intercept + s_slope * q_range

png("11.28d_supply_demand.png", width = 800, height = 600, res = 120)
plot(q_range, p_demand, type = "l", col = "blue", lwd = 2,
     xlim = c(0, 30), ylim = c(0, 120),
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Truffle Supply and Demand Curves (2SLS Estimates)")
lines(q_range, p_supply, col = "red", lwd = 2)
legend("topright", legend = c("Inverse Demand (2SLS)", "Inverse Supply (2SLS)"),
       col = c("blue", "red"), lwd = 2)
dev.off()
cat("圖片已儲存：11.28d_supply_demand.png\n")

# -------------------------------------------------------
# (e) 均衡 P & Q
# -------------------------------------------------------
# 令 d_intercept + d_slope*Q = s_intercept + s_slope*Q，解 Q
Q_eq <- (s_intercept - d_intercept) / (d_slope - s_slope)
P_eq <- d_intercept + d_slope * Q_eq

cat("\n===== (e) 結構型方程式均衡值 =====\n")
cat(sprintf("均衡 Q = %.4f\n", Q_eq))
cat(sprintf("均衡 P = %.4f\n", P_eq))

# 縮減型方程式預測（Table 11.2）
reduced_p <- lm(p ~ ps + di + pf, data = truffles)
reduced_q <- lm(q ~ ps + di + pf, data = truffles)
cat("\n--- 縮減型方程式 P ---\n"); print(summary(reduced_p))
cat("\n--- 縮減型方程式 Q ---\n"); print(summary(reduced_q))

new_data  <- data.frame(ps = PS_star, di = DI_star, pf = PF_star)
P_reduced <- predict(reduced_p, newdata = new_data)
Q_reduced <- predict(reduced_q, newdata = new_data)
cat(sprintf("\n縮減型預測：P = %.4f，Q = %.4f\n", P_reduced, Q_reduced))
cat(sprintf("結構型均衡：P = %.4f，Q = %.4f\n", P_eq, Q_eq))

# -------------------------------------------------------
# (f) OLS 估計反供需方程式，與 2SLS 比較
# -------------------------------------------------------
demand_ols <- lm(p ~ q + ps + di, data = truffles)
supply_ols <- lm(p ~ q + pf,     data = truffles)

cat("\n===== (f) OLS 反需求方程式 =====\n"); print(summary(demand_ols))
cat("\n===== (f) OLS 反供給方程式 =====\n"); print(summary(supply_ols))

cat("\n--- OLS vs 2SLS 係數比較（反需求）---\n")
comp_d <- cbind(OLS = coef(demand_ols), `2SLS` = coef(demand_2sls))
print(round(comp_d, 4))

cat("\n--- OLS vs 2SLS 係數比較（反供給）---\n")
comp_s <- cbind(OLS = coef(supply_ols), `2SLS` = coef(supply_2sls))
print(round(comp_s, 4))
