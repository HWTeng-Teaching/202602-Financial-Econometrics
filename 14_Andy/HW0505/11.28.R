# 1. 環境準備
rm(list=ls())
library(POE5Rdata)
library(AER)
data("truffles")
dat <- truffles

# --- (a) 重新寫出方程式 (代數轉換) ---
# 原本: Q = a1 + a2*P + a3*PS + a4*DI (需求)
# 改寫後: P = (1/a2)*Q - (a3/a2)*PS - (a4/a2)*DI - (a1/a2)
# 預期符號：需求曲線 P 與 Q 反向，故 Q 的係數應為負；供給曲線 P 與 Q 同向，故 Q 係數為正。

# --- (b) 使用 2SLS 估計改寫後的供需方程式 ---

# 逆需求曲線 (P 是左式, Q 是內生變數)
# 工具變數包含所有外生變數: ps, di, pf
fit_demand_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = dat)
summary(fit_demand_iv)

# 逆供給曲線 (P 是左式, Q 是內生變數)
fit_supply_iv <- ivreg(p ~ q + pf | ps + di + pf, data = dat)
summary(fit_supply_iv)

cat("\n(b) 2SLS 逆需求曲線結果:\n")
summary(fit_demand_iv)
cat("\n(b) 2SLS 逆供給曲線結果:\n")
summary(fit_supply_iv)

# --- (c) 計算「平均值處」的需求價格彈性 ---
# 注意：彈性公式為 (dQ/dP) * (P/Q)
# 在 P = f(Q) 的模型中，Q 的係數是 dP/dQ。所以 dQ/dP = 1 / (Q的係數)
b_q_demand <- coef(fit_demand_iv)["q"]
avg_p <- mean(dat$p)
avg_q <- mean(dat$q)
elasticity <- (1 / b_q_demand) * (avg_p / avg_q)
cat("\n(c) 在平均值處的需求價格彈性:", elasticity, "\n")

# --- (d) & (e) 計算均衡價格與數量 ---
# 設定外生變數數值
di_star <- 3.5
pf_star <- 23
ps_star <- 22

# 這裡通常透過解聯立方程或使用簡化式 (Reduced-form) 預測
# 我們跑簡化式來獲得預測值
red_p <- lm(p ~ ps + di + pf, data = dat)
red_q <- lm(q ~ ps + di + pf, data = dat)

new_data <- data.frame(ps = ps_star, di = di_star, pf = pf_star)
p_equil <- predict(red_p, new_data)
q_equil <- predict(red_q, new_data)

cat("\n(e) 預測均衡價格:", p_equil, "\n")
cat("\n(e) 預測均衡數量:", q_equil, "\n")

# 1. 提取 (b) 小題 2SLS 估計的係數(含所有的變數)
c_d <- coef(fit_demand_iv)
c_s <- coef(fit_supply_iv)
# 2. 設定 (d) 小題給定的外生變數固定值
DI_star <- 3.5
PF_star <- 23
PS_star <- 22
# 3. 計算截距項 (把固定值帶入方程式)
# 需求曲線: P = alpha1 + alpha_q*Q + alpha_ps*PS + alpha_di*DI
# 整理成 P = Intercept + Slope*Q 的形式
intercept_d <- c_d["(Intercept)"] + c_d["ps"] * PS_star + c_d["di"] * DI_star
slope_d     <- c_d["q"]
# 供給曲線: P = beta1 + beta_q*Q + beta_pf*PF
intercept_s <- c_s["(Intercept)"] + c_s["pf"] * PF_star
slope_s     <- c_s["q"]
# 4. 定義 Q 的範圍 (根據資料範圍設定，例如 0 到 100)
q_axis <- seq(min(dat$q), max(dat$q), length.out = 100)
# 5. 計算對應的 P 值(把不同的q值代入需求與供給函數找點)
p_demand <- intercept_d + slope_d * q_axis
p_supply <- intercept_s + slope_s * q_axis
# 6. 開始繪圖
plot(q_axis, p_demand, type = "l", col = "blue", lwd = 2,
     ylim = c(min(p_demand, p_supply), max(p_demand, p_supply)),
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Truffle Market: Supply and Demand Curves")
lines(q_axis, p_supply, col = "red", lwd = 2)
# 7. 標註均衡點 (e小題的結果)
# 這裡可以使用剛才計算的 p_equil 和 q_equil
points(q_equil, p_equil, col = "black", pch = 19)
text(q_equil, p_equil, labels = paste("Equilibrium (", round(q_equil,2), ",", round(p_equil,2), ")"), 
     pos = 3, cex = 0.8)
# 8. 加入圖例
legend("topright", legend = c("Demand", "Supply"), 
       col = c("blue", "red"), lwd = 2)
grid() # 加入格線方便觀察

#(e)使用 2SLS 係數來解聯立方程

# 1. 提取 2SLS 的結構係數 (從 b 小題的模型中)
c_d <- coef(fit_demand_iv)
c_s <- coef(fit_supply_iv)
# 2. 設定 (d) 小題給定的外生變項數值
PS_star <- 22
DI_star <- 3.5
PF_star <- 23
# 3. 計算兩條曲線各自的「常數項」(Intercept + 其他固定外生項)
# 需求曲線常數項 (A): alpha1 + alpha_ps*PS + alpha_di*DI
A <- c_d["(Intercept)"] + c_d["ps"] * PS_star + c_d["di"] * DI_star
# 供給曲線常數項 (B): beta1 + beta_pf*PF
B <- c_s["(Intercept)"] + c_s["pf"] * PF_star
# 4. 提取 Q 的斜率
slope_d <- c_d["q"] # 這是 alpha_Q
slope_s <- c_s["q"] # 這是 beta_Q
# 5. 解聯立方程 (A + slope_d * Q = B + slope_s * Q)
# 移項整理: Q * (slope_d - slope_s) = B - A
# 因此 Q = (B - A) / (slope_d - slope_s)
Q_star <- as.numeric((B - A) / (slope_d - slope_s))
# 6. 將 Q_star 帶回任一式求 P_star (這裡帶入需求式)
P_star <- as.numeric(A + slope_d * Q_star)
# 7. 輸出結果
cat("使用 2SLS 結構係數解出的均衡點：\n")
cat("均衡數量 Q* =", Q_star, "\n")
cat("均衡價格 P* =", P_star, "\n")

# --- (f) 使用 OLS 估計並比較 ---
fit_demand_ols <- lm(p ~ q + ps + di, data = dat)
fit_supply_ols <- lm(p ~ q + pf, data = dat)
summary(fit_demand_ols)
summary(fit_supply_ols)

cat("\n(f) OLS 與 2SLS 比較 (需求):\n")
print(cbind(OLS = coef(fit_demand_ols), IV = coef(fit_demand_iv)))
cat("\n(f) OLS 與 2SLS 比較 (供給):\n")
print(cbind(OLS = coef(fit_supply_ols), IV = coef(fit_supply_iv)))