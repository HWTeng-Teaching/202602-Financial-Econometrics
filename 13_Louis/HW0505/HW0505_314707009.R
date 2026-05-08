rm(list=ls()) #Removes all items in Environment!
library(systemfit)
library(broom) #for `glance(`) and `tidy()`
library(POE5Rdata) #for POE5 dataset
library(knitr) #for kable()

#10.24
data("fultonfish", package="POE5Rdata")

#a小題
# 1. 估計新的縮減式 (加入 mixed)
fishP_new.ols <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)

# 輸出整齊的報表觀察 mixed 的 p-value (查看是否 < 0.05)
kable(tidy(fishP_new.ols), digits = 4, caption = "New Reduced 'P' equation")

# 2. 檢定 STORMY 和 MIXED 的聯合顯著性
# 建立一個不包含 stormy 和 mixed 的受限模型
fishP_restricted_weather <- lm(lprice ~ mon + tue + wed + thu, data = fultonfish)

# 使用 anova() 進行 F 檢定
anova_weather <- anova(fishP_restricted_weather, fishP_new.ols)
print(anova_weather)

#b小題
# 1. 定義需求方程式 (內生變數 lprice 在右側)
fish.D <- lquan ~ lprice + mon + tue + wed + thu

# 2. 定義系統 (此處只需估計需求方程式，故 list 中只有一個)
eqs_b <- list(Demand = fish.D)

# 3. 定義所有的外生變數與工具變數 (加入 mixed)
ivs_b <- ~ mon + tue + wed + thu + stormy + mixed

# 4. 執行 2SLS 估計
sys_b <- systemfit(eqs_b, method = "2SLS", inst = ivs_b, data = fultonfish)

# 觀察估計結果，比較 lprice 係數與課本 Table 11.5 的差異
summary(sys_b)

#c小題
# 1. 取得 2SLS 需求方程式的殘差
res_demand <- residuals(sys_b$eq[[1]])

# 2. 將殘差對「所有的外生與工具變數」進行輔助迴歸
sargan_aux <- lm(res_demand ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)

# 3. 計算檢定統計量: n * R-squared (服從卡方分配)
r_sq <- summary(sargan_aux)$r.squared
n_obs <- nobs(sargan_aux)
sargan_stat <- n_obs * r_sq

# 4. 計算 p-value
# 自由度 (df) = 工具變數總數 (2) - 內生解釋變數數量 (1) = 1
p_val <- 1 - pchisq(sargan_stat, df = 1)

# 輸出 Sargan 檢定結果
cat("Sargan Test Statistic:", sargan_stat, "\n")
cat("P-value:", p_val, "\n")

#d小題
# 在 part (a) 的縮減式 fishP_new.ols 中，檢定四天星期變數的聯合顯著性
# 建立一個不包含星期變數的受限模型
fishP_restricted_days <- lm(lprice ~ stormy + mixed, data = fultonfish)

# 使用 anova() 進行 F 檢定
anova_days <- anova(fishP_restricted_days, fishP_new.ols)
print(anova_days)


#10.28
#a小題無需
#b小題
data("truffles", package="POE5Rdata")
# 定義反需求與反供給方程式 (P 在左邊)
inv_D <- p ~ q + ps + di
inv_S <- p ~ q + pf

# 打包系統與定義工具變數清單
eqs_inv <- list(InvDemand = inv_D, InvSupply = inv_S)
insts_all <- ~ ps + di + pf

# 執行 2SLS
sys_inv_2sls <- systemfit(eqs_inv, method = "2SLS", inst = insts_all, data = truffles)
summary(sys_inv_2sls)

#c小題
# 取得反需求函數中 Q 的估計係數
coef_q_invD <- coef(sys_inv_2sls$eq[[1]])["q"]

# 計算 P 和 Q 的樣本平均值
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)

# 計算平均值處的彈性
elasticity <- (1 / coef_q_invD) * (mean_p / mean_q)
cat("Price Elasticity of Demand at the means:", elasticity, "\n")

#d小題
# 提取係數
cD <- coef(sys_inv_2sls$eq[[1]]) # 反需求係數
cS <- coef(sys_inv_2sls$eq[[2]]) # 反供給係數

# 設定外生變數數值
val_di <- 3.5
val_ps <- 22
val_pf <- 23

# 計算固定外生變數後的截距
int_D <- cD["(Intercept)"] + cD["ps"] * val_ps + cD["di"] * val_di
slope_D <- cD["q"]

int_S <- cS["(Intercept)"] + cS["pf"] * val_pf
slope_S <- cS["q"]

cat("Demand Equation for Sketch: P =", int_D, "+ (", slope_D, ") * Q \n")
cat("Supply Equation for Sketch: P =", int_S, "+ (", slope_S, ") * Q \n")

#畫圖
library(ggplot2)
# 1. 建立一組 Q 的數值來畫圖 (假設從 0 到 30)
q_values <- data.frame(Q = seq(0, 30, length.out = 100))

# 2. 使用 ggplot2 繪製供需曲線
ggplot(q_values, aes(x = Q)) +
  # 畫出需求曲線 (藍色)
  stat_function(fun = function(q) int_D + slope_D * q, 
                aes(color = "Demand"), size = 1.2) +
  # 畫出供給曲線 (紅色)
  stat_function(fun = function(q) int_S + slope_S * q, 
                aes(color = "Supply"), size = 1.2) +
  # 設定顏色與標籤
  scale_color_manual(values = c("Demand" = "blue", "Supply" = "red")) +
  labs(title = "Truffle Market: Supply and Demand Curves",
       subtitle = paste("Given: DI =", val_di, ", PS =", val_ps, ", PF =", val_pf),
       x = "Quantity (Q)",
       y = "Price (P)",
       color = "Curve") +
  theme_minimal() +
  # 設定 X 軸與 Y 軸的顯示範圍 (若跑出來的圖太空，可自行調整 ylim 與 xlim)
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 150))

#e小題
# 方法一：利用 (d) 小題算出的 2SLS 結構方程式來解聯立
# 令 P_Demand = P_Supply，即 int_D + slope_D * Q = int_S + slope_S * Q
# 移項求解 Q*
q_star_struct <- (int_S - int_D) / (slope_D - slope_S)
# 將 Q* 代回任一方程式求 P* (這裡代回需求方程式)
p_star_struct <- int_D + slope_D * q_star_struct

cat("=== 方法一：結構方程式 (2SLS) 解出之均衡點 ===\n")
cat("預測均衡數量 Q* =", q_star_struct, "\n")
cat("預測均衡價格 P* =", p_star_struct, "\n\n")

# 方法二：利用 Table 11.2 的縮減式 (Reduced-form) 進行預測
# 重新跑一次老師自主練習碼中的縮減式 (確保環境中有模型)
Q.red <- lm(q ~ ps + di + pf, data = truffles)
P.red <- lm(p ~ ps + di + pf, data = truffles)

# 建立一個包含題目給定外生變數數值的資料框 (Data frame)
new_obs <- data.frame(ps = 22, di = 3.5, pf = 23)

# 使用 predict() 函數直接算出對應的 Q* 與 P*
q_star_red <- predict(Q.red, newdata = new_obs)
p_star_red <- predict(P.red, newdata = new_obs)

cat("=== 方法二：縮減式 (Reduced-form) 預測之均衡點 ===\n")
cat("預測均衡數量 Q* =", q_star_red, "\n")
cat("預測均衡價格 P* =", p_star_red, "\n")

#f小題
# 使用 OLS 估計
ols_inv_D <- lm(inv_D, data = truffles)
ols_inv_S <- lm(inv_S, data = truffles)

# 觀察並比較結果
summary(ols_inv_D)
summary(ols_inv_S)
