setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("truffles.rdata")
library(AER)       

# (b)
demand_2sls <- ivreg(p ~ q + ps + di |
                       ps + di + pf,
                     data = truffles)
summary(demand_2sls)

supply_2sls <- ivreg(p ~ q + pf |
                       ps + di + pf,
                     data = truffles)
summary(supply_2sls)


# (c)
mean_p <- mean(truffles$p)   # P 的樣本平均數
mean_q <- mean(truffles$q)   # Q 的樣本平均數

delta2 <- coef(demand_2sls)["q"]  # 取出 δ₂ 的估計值（Q 的係數)
elasticity <- (1 / delta2) * (mean_p / mean_q)  # 1/delta2：將 ∂P/∂Q 倒數得到 ∂Q/∂P
cat("需求價格彈性（at the means）:", elasticity, "\n")


# (d)
coef_d <- coef(demand_2sls)
coef_s <- coef(supply_2sls)
DI_star <- 3.5
PS_star <- 22
PF_star <- 23

intercept_D <- coef_d["(Intercept)"] +
  coef_d["ps"] * PS_star +
  coef_d["di"] * DI_star

intercept_S <- coef_s["(Intercept)"] +
  coef_s["pf"] * PF_star

q_range <- seq(0, 35, length.out = 100)

P_demand <- intercept_D + coef_d["q"] * q_range
P_supply <- intercept_S + coef_s["q"] * q_range

plot(q_range, P_demand,
     type = "l",        # type="l"：畫折線圖（line）
     col  = "blue",     # 需求曲線用藍色
     lwd  = 2,          # lwd：線條粗細
     xlab = "Quantity (Q)",   # x 軸標籤
     ylab = "Price (P)",      # y 軸標籤
     main = "Truffle Supply and Demand",  # 圖標題
     ylim = c(0, 120))  # ylim：y 軸範圍

lines(q_range, P_supply,
      col = "red",   # 供給曲線用紅色
      lwd = 2)

legend("topright",
       legend = c("Demand", "Supply"),
       col    = c("blue", "red"),
       lwd    = 2)


# (e)
Q_star <- (intercept_S - intercept_D) /
  (coef_d["q"] - coef_s["q"])
# 聯立兩方程式求解 Q*
# 分子：兩截距之差
# 分母：兩斜率（Q 的係數）之差

P_star <- intercept_D + coef_d["q"] * Q_star
# 將 Q* 代回需求方程式求 P*

cat("結構式均衡 Q*:", Q_star, "\n")
cat("結構式均衡 P*:", P_star, "\n")

# 簡化式直接用外生變數預測均衡值
reduced_P <- lm(p ~ ps + di + pf, data = truffles)
reduced_Q <- lm(q ~ ps + di + pf, data = truffles)
# p 和 q 分別對所有外生變數回歸

# 建立新資料點（代入固定外生變數值）
new_data <- data.frame(ps = PS_star, di = DI_star, pf = PF_star)
# data.frame()：建立只有一列的資料框，包含外生變數的固定值

P_reduced <- predict(reduced_P, newdata = new_data)
Q_reduced <- predict(reduced_Q, newdata = new_data)
# predict()：用估計好的模型對新資料進行預測
# newdata：指定要預測的新觀測值

cat("簡化式均衡 P*:", P_reduced, "\n")
cat("簡化式均衡 Q*:", Q_reduced, "\n")


# (f)
demand_ols <- lm(p ~ q + ps + di, data = truffles)
# lm()：直接 OLS，不處理 Q 的內生性問題
# 這是「錯誤」的做法，但用來與 2SLS 比較
summary(demand_ols)

supply_ols <- lm(p ~ q + pf, data = truffles)
summary(supply_ols)

# ── 比較 OLS 與 2SLS 的係數 ─────────────────────────────
comparison <- data.frame(
  Parameter = c("Intercept", "Q", "PS", "DI"),
  OLS  = coef(demand_ols),
  TSLS = coef(demand_2sls)
)
# data.frame()：建立比較表格
# coef()：提取各模型的係數向量

print(comparison)
# 預期：OLS 的 Q 係數偏誤
# 因為 Q 與誤差項相關，OLS 高估或低估真實參數
# 2SLS 透過工具變數修正了這個偏誤

