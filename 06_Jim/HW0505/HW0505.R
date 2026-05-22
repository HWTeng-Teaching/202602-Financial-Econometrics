rm(list=ls())
library(POE5Rdata)
library(stargazer)
# stargazer(results, summary=FALSE, type="latex", 
#           title="Simulation Results", 
#           header=FALSE)
library(ggplot2)
library(gridExtra)
library(AER)
library(car)

#24
data(fultonfish)



#=============================================================================
# a. 估計新的 ln(PRICE) 簡化式，並進行統計檢定
#=============================================================================

# 估計包含 STORMY 與 MIXED 的 ln(PRICE) 簡化式方程 (OLS)
rf_price <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary(rf_price)
# 印出簡化式結果，以便觀察 MIXED 是否在 5% 水準下單獨顯著
stargazer(rf_price, summary=FALSE, type="latex",
          header=FALSE)

# 檢定 STORMY 與 MIXED 的排除限制（聯合顯著性檢定）
f_test_instruments <- linearHypothesis(rf_price, c("stormy = 0", "mixed = 0"))
print(f_test_instruments)
stargazer(f_test_instruments, summary=FALSE, type="latex",
          title="Simulation Results",
          header=FALSE)



#=============================================================================
# b. & c. 使用 2SLS 估計需求方程，與 Table 11.5 比較，並執行 Sargan 檢定
#=============================================================================

# 使用 ivreg 進行 2SLS 估計
# 格式為：因變數 ~ 結構式解釋變數 | 所有外生變數與工具變數
tsls_demand <- ivreg(lquan ~ lprice + mon + tue + wed + thu | 
                       mon + tue + wed + thu + stormy + mixed, data = fultonfish)

# 印出 2SLS 估計結果與原課本 Table 11.5（恰好識別版本）進行對比
stargazer(tsls_demand, type = "latex", title = "2SLS Estimates for Fish Demand (Overidentified)")

# 執行診斷檢定（包含弱工具變數檢定、Wu-Hausman 內生性檢定、Sargan 過度識別檢定）
# diagnostics = TRUE 會在 summary 中直接輸出這些檢定
tsls_summary <- summary(tsls_demand, diagnostics = TRUE)
print(tsls_summary)

# 提取並單獨展示 Sargan 檢定結果
sargan_results <- tsls_summary$diagnostics["Sargan", ]
cat("\n[Part c] Sargan 過度識別檢定結果:\n")
print(sargan_results)


#=============================================================================
# d. 在簡化式中檢定星期變數的聯合顯著性，評估供給方程的可估計性
#=============================================================================

# 在 part (a) 的簡化式模型中，檢定 mon, tue, wed, thu 是否聯合為 0
f_test_days <- linearHypothesis(rf_price, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
print(f_test_days)

# 提取星期變數聯合檢定的 p-value
p_val_days <- f_test_days$`Pr(>F)`[2]



#28
data(truffles)

# ============================================================================
# Part (b) & (c): 兩階段最小平方法 (2SLS) 估計與彈性計算
# ============================================================================

# --- 2SLS 需求曲線 (Inverse Demand) ---
# 內生變數: q, 外生變數/工具變數: ps, di, pf
tsls_demand <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
print(summary(tsls_demand))
stargazer(tsls_demand, summary=FALSE, type="latex",
          title="tsls_demand",
          header=FALSE)

# --- 2SLS 供給曲線 (Inverse Supply) ---
# 內生變數: q, 外生變數/工具變數: ps, di, pf
tsls_supply <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
print(summary(tsls_supply))
stargazer(tsls_supply, summary=FALSE, type="latex",
          title="tsls_supply",
          header=FALSE)

# --- Part (c): 計算平均值處的需求價格彈性 ---
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)
delta_2 <- coef(tsls_demand)["q"] # 提取小寫 q 的係數

# 彈性公式: (1 / delta_2) * (mean_p / mean_q)
price_elasticity <- (1 / delta_2) * (mean_p / mean_q)
cat(sprintf("\nPrice Elasticity of Demand at the means: %.4f\n", price_elasticity))


# ==============================================================================
# Part (d) & (e): 繪圖與均衡值求解
# ==============================================================================

# 設定題目指定的外生變數值
di_star <- 3.5
pf_star <- 23
ps_star <- 22

# 提取 2SLS 估計係數
d_coef <- coef(tsls_demand)
s_coef <- coef(tsls_supply)

# 計算簡化供需曲線的截距 (p = intercept + slope * q)
demand_intercept <- d_coef["(Intercept)"] + d_coef["ps"] * ps_star + d_coef["di"] * di_star
demand_slope     <- d_coef["q"]

supply_intercept <- s_coef["(Intercept)"] + s_coef["pf"] * pf_star
supply_slope     <- s_coef["q"]

# --- Part (d): 繪製供需曲線圖 ---
# 定義數量的繪圖範圍
q_axis <- seq(0, 35, by = 0.1)
p_demand <- demand_intercept + demand_slope * q_axis
p_supply <- supply_intercept + supply_slope * q_axis

# 3. 調整 ylim 至 c(-10, 120)，完美容納截距 111.57
plot(q_axis, p_demand, type = "l", col = "blue", lwd = 2, ylim = c(-10, 120),
     xlab = "Quantity (q)", ylab = "Price (p)", 
     main = "Truffle Market: Inverse Supply and Demand")

# 4. 疊加供給曲線
lines(q_axis, p_supply, col = "red", lwd = 2)

# 5. 加上輔助線
abline(h = 0, v = 0, col = "gray60", lty = 2)

# 6. 標記結構式均衡點（此時點會完美呈現在圖表中央偏右上方）
points(q_eq_structural, p_eq_structural, col = "purple", pch = 19, cex = 1.5)
text(q_eq_structural, p_eq_structural + 6, "Equilibrium", col = "purple", font = 2)

# 7. 修正圖例位置避免遮擋圖形
legend("top", legend = c("Inverse Demand", "Inverse Supply"), 
       col = c("blue", "red"), lty = 1, lwd = 2, 
       horiz = TRUE, cex = 0.8, bty = "n")

# --- Part (e): 結構式聯立求解均衡值 ---
# demand_intercept + demand_slope * q = supply_intercept + supply_slope * q
q_eq_structural <- (demand_intercept - supply_intercept) / (supply_slope - demand_slope)
p_eq_structural <- demand_intercept + demand_slope * q_eq_structural

cat("\n=== Equilibrium from 2SLS Structural Equations ===\n")
cat(sprintf("q* = %.4f, p* = %.4f\n", q_eq_structural, p_eq_structural))

# --- Part (e): 課本 Table 11.2 縮減式 (Reduced-Form) 預測值 ---
# 這裡手動輸入課本 Table 11.2 已估計好的縮減式參數
q_reduced <- 7.8951 + 0.6564 * ps_star + 2.1672 * di_star - 0.5070 * pf_star
p_reduced <- -32.5124 + 1.7081 * ps_star + 7.6025 * di_star + 1.3539 * pf_star

cat("\n=== Prediction from Table 11.2 Reduced-Form Equations ===\n")
cat(sprintf("q_reduced = %.4f, p_reduced = %.4f\n", q_reduced, p_reduced))




# ==============================================================================
# Part (f): 普通最小平方法 (OLS) 估計與比較
# ==============================================================================

ols_demand <- lm(p ~ q + ps + di, data = truffles)
cat("\n=== OLS Inverse Demand Summary ===\n")
print(summary(ols_demand))
stargazer(ols_demand, summary=FALSE, type="latex",
          title="ols demand",
          header=FALSE)

ols_supply <- lm(p ~ q + pf, data = truffles)
cat("\n=== OLS Inverse Supply Summary ===\n")
print(summary(ols_supply))
print(summary(ols_supply))
stargazer(ols_supply, summary=FALSE, type="latex",
          title="ols supply",
          header=FALSE)
