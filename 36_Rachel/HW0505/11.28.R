if (!require(AER)) {
  install.packages("AER")
  library(AER)
}
data("truffles")

#b
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
print(summary(demand_2sls, diagnostics = TRUE))

supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
print(summary(supply_2sls, diagnostics = TRUE))

#c
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)
gamma_2 <- coef(demand_2sls)["q"]
elasticity <- (1 / gamma_2) * (mean_p / mean_q)
cat("=========================================\n")
cat(" (c) 需求價格彈性計算結果 (於平均值處)\n")
cat("=========================================\n")
cat(sprintf("1. 樣本平均價格 (P_bar) : %.4f\n", mean_p))
cat(sprintf("2. 樣本平均數量 (Q_bar) : %.4f\n", mean_q))
cat(sprintf("3. 反需求迴歸斜率 (dP/dQ): %.4f\n", gamma_2))
cat(sprintf("4. 邊際效果 (dQ/dP)      : %.4f\n", 1 / gamma_2))
cat(sprintf("-----------------------------------------\n"))
cat(sprintf("▶ 需求價格彈性 (E_d)   : %.4f\n", elasticity))

#d
PS_star <- 22
DI_star <- 3.5
PF_star <- 23

# 從模型提取係數並計算固定後的「需求線截距」
# 需求截距 = beta_0 + beta_PS * PS* + beta_DI * DI*
coef_d <- coef(demand_2sls)
int_demand <- coef_d["(Intercept)"] + coef_d["ps"] * PS_star + coef_d["di"] * DI_star
slope_demand <- coef_d["q"]
coef_s <- coef(supply_2sls)
int_supply <- coef_s["(Intercept)"] + coef_s["pf"] * PF_star
slope_supply <- coef_s["q"]

# 計算均衡點 (讓兩條線相交：Pd = Ps)
q_eq <- (int_supply - int_demand) / (slope_demand - slope_supply)
p_eq <- int_demand + slope_demand * q_eq

# 建立空白畫布 (設定 X 軸與 Y 軸範圍)
plot(NULL, xlim=c(0, 30), ylim=c(0, 120),
     xlab="Quantity of Truffles (Q)", ylab="Price of Truffles (P)", 
     main="Supply and Demand for Truffles\n(PS=22, DI=3.5, PF=23)",
     las=1) # las=1 讓 Y 軸數字轉正
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
abline(a = int_demand, b = slope_demand, col = "blue", lwd = 3)
abline(a = int_supply, b = slope_supply, col = "red", lwd = 3)
points(q_eq, p_eq, pch = 19, col = "black", cex = 1.5)
legend("topright", legend=c("Demand", "Supply"), 
       col=c("blue", "red"), lwd=3, bg="white")

cat("繪圖完成！請查看 RStudio 的 Plots 視窗。\n")
cat(sprintf("固定後需求線: P = %.4f %.4f Q\n", int_demand, slope_demand))
cat(sprintf("固定後供給線: P = %.4f + %.4f Q\n", int_supply, slope_supply))

#e
# Table 11.2 中 Q 的係數
coef_Q_table <- c(Intercept = 7.8951, PS = 0.6564, DI = 2.1672, PF = -0.5070)
# 計算預測均衡數量
Q_pred_table <- coef_Q_table["Intercept"] + 
  coef_Q_table["PS"] * PS_star + 
  coef_Q_table["DI"] * DI_star + 
  coef_Q_table["PF"] * PF_star
# Table 11.2 中 P 的係數
coef_P_table <- c(Intercept = -32.5124, PS = 1.7081, DI = 7.6653, PF = 1.2891)
# 計算預測均衡價格
P_pred_table <- coef_P_table["Intercept"] + 
  coef_P_table["PS"] * PS_star + 
  coef_P_table["DI"] * DI_star + 
  coef_P_table["PF"] * PF_star
cat("=== 根據 Table 11.2 係數計算的結果 ===\n")
cat(sprintf("預測均衡數量 Q* = %.4f\n", Q_pred_table))
cat(sprintf("預測均衡價格 P* = %.4f\n\n", P_pred_table))

# 執行縮減式 OLS 迴歸：內生變數對所有外生變數迴歸
reduced_form_Q <- lm(q ~ ps + di + pf, data = truffles)
reduced_form_P <- lm(p ~ ps + di + pf, data = truffles)
cat("=== 實際資料跑出的縮減式係數 (驗證 Table 11.2) ===\n")
cat("依變數 Q 的係數:\n")
print(round(coef(reduced_form_Q), 4))
cat("\n依變數 P 的係數:\n")
print(round(coef(reduced_form_P), 4))

# 建立預測用的新資料框
newdata <- data.frame(ps = PS_star, di = DI_star, pf = PF_star)

# 使用模型進行預測
Q_pred_data <- predict(reduced_form_Q, newdata)
P_pred_data <- predict(reduced_form_P, newdata)

cat("\n=== 使用資料模型預測的結果 ===\n")
cat(sprintf("預測均衡數量 Q* = %.4f\n", Q_pred_data))
cat(sprintf("預測均衡價格 P* = %.4f\n", P_pred_data))
cat("--------------------------------------------------\n")
cat("結論：這組答案應與您在 (d) 小題從結構式推導的交點高度一致。\n")

#f
demand_ols <- lm(p ~ q + ps + di, data = truffles)
supply_ols <- lm(p ~ q + pf, data = truffles)
print(summary(demand_ols))
print(summary(supply_ols))

demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)

cat("\n=========================================\n")
cat(" ★ 核心比較：OLS vs 2SLS 的 Q 係數 ★\n")
cat("=========================================\n")
cat(sprintf("【需求方程式中 Q 的斜率】\n"))
cat(sprintf("  - 錯誤的 OLS 估計 : %8.4f (產生聯立偏誤)\n", coef(demand_ols)["q"]))
cat(sprintf("  - 正確的 2SLS 估計: %8.4f (符合需求法則)\n\n", coef(demand_2sls)["q"]))

cat(sprintf("【供給方程式中 Q 的斜率】\n"))
cat(sprintf("  - 偏誤的 OLS 估計 : %8.4f \n", coef(supply_ols)["q"]))
cat(sprintf("  - 正確的 2SLS 估計: %8.4f \n", coef(supply_2sls)["q"]))

