

# 1. 載入套件與資料處理 --------------------------------------
if (!require("AER")) install.packages("AER") 
if (!require("ggplot2")) install.packages("ggplot2")

library(AER)
library(ggplot2)

url <- "http://www.principlesofeconometrics.com/poe5/data/csv/truffles.csv"
truffles <- read.csv(url)


if (exists("truffles")) {
  print("資料讀取成功！")
  head(truffles)
}

# 2. (b) 2SLS 估計 -----------------------------------------
# 需求：P = f(Q, PS, DI)，內生變數 Q，工具變數 PF
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)

# 供給：P = f(Q, PF)，內生變數 Q，工具變數 PS, DI
supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)

summary(demand_2sls)
summary(supply_2sls)


# 3. (c) 計算平均值處的需求價格彈性 ---------------------------
a2_hat <- coef(demand_2sls)["q"]
p_mean <- mean(truffles$p)
q_mean <- mean(truffles$q)

# 彈性 = (dQ/dP) * (P/Q)
elasticity_d <- (1 / a2_hat) * (p_mean / q_mean)
cat("\n需求價格彈性 (at means):", round(elasticity_d, 4), "\n")


# 4. (d) 繪製供需均衡圖 --------------------------------------
# 設定題目給定的外生變數值
DI_star <- 3.5
PF_star <- 23
PS_star <- 22

# 計算截距 (根據 a 小題改寫的方程式)
d_int <- coef(demand_2sls)[1] + coef(demand_2sls)["ps"]*PS_star + coef(demand_2sls)["di"]*DI_star
s_int <- coef(supply_2sls)[1] + coef(supply_2sls)["pf"]*PF_star

#ggplot2 裡，用 + 來 「疊加圖層」
#aes(color = "Supply") 歸類做 'Demand' 的組別，並根據這個組別自動分配顏色
ggplot(data.frame(x = c(15, 35)), aes(x = x)) +
  stat_function(fun = function(q) d_int + coef(demand_2sls)["q"]*q, aes(color = "Demand")) +
  stat_function(fun = function(q) s_int + coef(supply_2sls)["q"]*q, aes(color = "Supply")) +
  labs(title = "Truffle Market Equilibrium ",
       x = "Quantity (Q)", y = "Price (P)", color = "Curve Type") +
  theme_minimal()

# ==========================================
# (e) 小題：計算均衡值 (Equilibrium)
# ==========================================

# 方法 1：使用 (d) 小題的結構係數解聯立方程
# 需求：P = d_int + d_slope * Q
# 供給：P = s_int + s_slope * Q
# d_int + d_slope * Q = s_int + s_slope * Q
# 解 Q = (s_int - d_int) / (d_slope - s_slope)

d_slope <- coef(demand_2sls)["q"]
s_slope <- coef(supply_2sls)["q"]

Q_eq <- (s_int - d_int) / (d_slope - s_slope)
P_eq <- d_int + d_slope * Q_eq

cat("--- (e) 結構方程式得到的均衡值 ---\n")
cat("均衡數量 Q* =", round(Q_eq, 4), "\n")
cat("均衡價格 P* =", round(P_eq, 4), "\n")

# 方法 2：使用簡化式 (Reduced-form) 預測
# 對 Q 和 P 分別做外生變數 (PS, DI, PF) 的 OLS

# 建立預測用的新資料點
new_data <- data.frame(ps = 22, di = 3.5, pf = 23)

# 估計簡化式模型 (Reduced Form)
rf_q <- lm(q ~ ps + di + pf, data = truffles)
rf_p <- lm(p ~ ps + di + pf, data = truffles)

Q_rf <- predict(rf_q, newdata = new_data)
P_rf <- predict(rf_p, newdata = new_data)

cat("\n--- (e) 簡化式模型預測的均衡值 ---\n")
cat("預測 Q* =", round(Q_rf, 4), "\n")
cat("預測 P* =", round(P_rf, 4), "\n")

# --- (f) 使用 OLS 估計需求與供給方程式 ---

# 需求方程式 OLS 估計
demand_ols <- lm(p ~ q + ps + di, data = truffles)

# 供給方程式 OLS 估計
supply_ols <- lm(p ~ q + pf, data = truffles)

# --- 結果對照表 ---
cat("--- 需求曲線 Q 係數比較 ---\n")
cat("2SLS 估計值:", coef(demand_2sls)["q"], "\n")
cat("OLS  估計值:", coef(demand_ols)["q"], "\n")

cat("\n--- 供給曲線 Q 係數比較 ---\n")
cat("2SLS 估計值:", coef(supply_2sls)["q"], "\n")
cat("OLS  估計值:", coef(supply_ols)["q"], "\n")

summary(demand_ols)
summary(supply_ols)
