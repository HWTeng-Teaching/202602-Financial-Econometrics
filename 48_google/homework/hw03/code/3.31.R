# 清除記憶體
rm(list=ls())

# 載入 POE5Rdata 套件
library(POE5Rdata)

# 呼叫特定數據集
data("tuna")

# 顯示數據統計摘要 (確認變數名)
summary(tuna)

# --- (a) 敘述統計與時間趨勢圖 ---
# 計算 sal1 與 apr1 的統計量
stats_sal1 <- c(mean=mean(tuna$sal1), min=min(tuna$sal1), max=max(tuna$sal1), sd=sd(tuna$sal1))
stats_apr1 <- c(mean=mean(tuna$apr1), min=min(tuna$apr1), max=max(tuna$apr1), sd=sd(tuna$apr1))

print("SAL1 敘述統計:")
print(stats_sal1)
print("APR1 敘述統計:")
print(stats_apr1)

# 修正：使用 1:52 代表週別繪圖
par(mfrow=c(2,1))
weeks <- 1:nrow(tuna)
plot(weeks, tuna$sal1, type="o", col="blue", main="Weekly Sales (sal1)", xlab="Week", ylab="Sales")
plot(weeks, tuna$apr1, type="o", col="red", main="Weekly Price (apr1)", xlab="Week", ylab="Price ($)")

# --- (b) 散佈圖分析 ---
par(mfrow=c(1,1))
plot(tuna$apr1, tuna$sal1, pch=19, col="darkgreen", 
     main="Sales vs Price", xlab="Price (apr1)", ylab="Sales (sal1)")
# 這裡可以看到兩者呈現負相關，符合經濟學需求定律

# --- (c) 線性回歸與單位轉換 ---
# 建立新變數 price1 (美分，題目要求 100 * apr1)
tuna$price1 <- 100 * tuna$apr1

# 估計模型: sal1 = b1 + b2*price1 + e
model_tuna <- lm(sal1 ~ price1, data = tuna)
model_sum <- summary(model_tuna)
print(model_sum)

# 95% 信賴區間 (價格每增加 1 美分的影響)
ci_beta2 <- confint(model_tuna, "price1", level = 0.95)

# --- (d) 價格為 70 美分時的預期銷售量 (90% 區間) ---
pred_70 <- predict(model_tuna, newdata = data.frame(price1 = 70), interval = "confidence", level = 0.90)

# --- (e) 平均值點的彈性 (Elasticity at the means) ---
# 彈性 epsilon = b2 * (avg_price1 / avg_sal1)
avg_p1 <- mean(tuna$price1)
avg_s1 <- mean(tuna$sal1)
b2_hat <- coef(model_tuna)["price1"]
se_b2 <- model_sum$coefficients["price1", "Std. Error"]

elast_mean <- b2_hat * (avg_p1 / avg_s1)
# SE(彈性) = SE(b2) * (avg_p1 / avg_s1)
se_elast <- se_b2 * (avg_p1 / avg_s1)
# 95% 彈性信賴區間
t_crit_95 <- qt(0.975, df.residual(model_tuna))
ci_elast <- elast_mean + c(-1, 1) * t_crit_95 * se_elast

# --- (f) 彈性是否等於 -3 的假設檢定 (10% 顯著水準) ---
# H0: elasticity = -3  vs  H1: elasticity != -3
t_stat_f <- (elast_mean - (-3)) / se_elast
p_val_f <- 2 * pt(abs(t_stat_f), df.residual(model_tuna), lower.tail = FALSE)

# ==========================================
# 輸出最後結果
# ==========================================
cat("\n--- 3.31 統計結果報告 ---\n")
cat("(c) price1 係數估計值:", b2_hat, "\n")
cat("    price1 95% 信賴區間: [", ci_beta2[1], ",", ci_beta2[2], "]\n")
cat("(d) Price=70 時預期銷售 90% CI: [", pred_70[2], ",", pred_70[3], "]\n")
cat("(e) 平均值點彈性:", elast_mean, "\n")
cat("    彈性 95% 信賴區間: [", ci_elast[1], ",", ci_elast[2], "]\n")
cat("(f) 檢定彈性等於 -3: t =", t_stat_f, ", p-value =", p_val_f, "\n")
