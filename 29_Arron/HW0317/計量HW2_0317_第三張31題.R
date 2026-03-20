url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"
download.file(url, destfile = "temp_tuna.rdata", mode = "wb")
load("temp_tuna.rdata")
ls()
head(tuna)

# 手動建立週別變數 (因為資料內沒有 week 欄位)
tuna$week_id <- 1:nrow(tuna)

# ==========================================
# (a) 敘述統計與趨勢圖
# ==========================================
# 1. 算統計量：平均、最小值、最大值、標準差
summary_stats <- summary(tuna[c("sal1", "apr1")])
sd_sal1 <- sd(tuna$sal1)
sd_apr1 <- sd(tuna$apr1)

mean_sal1 <- mean(tuna$sal1)
mean_apr1 <- mean(tuna$apr1)

cv_sal1 <- sd_sal1 / mean_sal1
cv_apr1 <- sd_apr1 / mean_apr1

print(summary_stats)
cat("SAL1 標準差:", sd_sal1, "\n")
cat("APR1 標準差:", sd_apr1, "\n")
cat("  變異係數 (CV):", round(cv_sal1, 4), "\n\n")
cat("  變異係數 (CV):", round(cv_apr1, 4), "\n")

# 2. 畫隨週別變化的趨勢圖
par(mfrow=c(1,2)) # 畫面切一半
plot(tuna$week_id, tuna$sal1, type="o", col="blue", 
     main="Weekly Sales (sal1)", xlab="Week", ylab="Sales")
plot(tuna$week_id, tuna$apr1, type="o", col="red", 
     main="Weekly Price (apr1)", xlab="Week", ylab="Price")

# ==========================================
# (b) 散佈圖與關係判斷
# ==========================================
par(mfrow=c(1,1)) # 恢復單一畫面
plot(tuna$apr1, tuna$sal1, pch=16, col="darkgreen",
     main="Scatter Plot: Price vs Sales", 
     xlab="Price (apr1)", ylab="Sales (sal1)")

# 經濟學解釋：這是一個反向關係 (Inverse Relationship)，符合需求法則。

# ==========================================
# (c) 線性回歸與 95% 信賴區間
# ==========================================
# 建立 price1 (單位：美分)
tuna$price1 <- 100 * tuna$apr1
model_tuna <- lm(sal1 ~ price1, data = tuna)
sum_tuna <- summary(model_tuna)

# 提取斜率 b2 的點估計與標準誤
b2_hat <- sum_tuna$coefficients["price1", "Estimate"]
se_b2 <- sum_tuna$coefficients["price1", "Std. Error"]

# 計算 95% 信賴區間
t_crit_95 <- qt(0.975, df = df.residual(model_tuna))
conf_b2 <- c(b2_hat - t_crit_95 * se_b2, b2_hat + t_crit_95 * se_b2)

print("--- (c) Regression Results (Price Effect) ---")
cat("點估計值 (每增加1美分影響):", b2_hat, "\n")
cat("95% 信賴區間: [", conf_b2[1], ",", conf_b2[2], "]\n")

# ==========================================
# (d) 預期銷量預測 (70美分) 與 90% 區間
# ==========================================
# 建立規格表 (newdata)
new_test <- data.frame(price1 = 70)

# 進行預測：設定 interval 為 confidence，信心水準為 0.90
pred_d <- predict(model_tuna, newdata = new_test, interval = "confidence", level = 0.90)

print("--- (d) Prediction at 70 Cents ---")
cat("預期銷量 (fit):", pred_d[1, "fit"], "\n")
cat("90% 信賴區間: [", pred_d[1, "lwr"], ",", pred_d[1, "upr"], "]\n")


# ==========================================
# (e) 計算「平均值處」的價格彈性 
# 彈性公式: Elasticity = b2 * (mean_price / mean_sales)

avg_price <- mean(tuna$price1)
avg_sales <- mean(tuna$sal1)

elasticity_hat <- b2_hat * (avg_price / avg_sales)
conf_elasticity <- conf_b2 * (avg_price / avg_sales)

cat("--- (e) Elasticity Results ---\n")
cat("平均值處的彈性估計值:", elasticity_hat, "\n")
cat("95% 彈性信賴區間: [", conf_elasticity[1], ",", conf_elasticity[2], "]\n")

# ==========================================
# (f) 假設檢定：檢定彈性是否等於 -3
# H0: Elasticity = -3
# H1: Elasticity != -3 (顯著水準 10%)

null_elasticity <- -3
se_elasticity <- se_b2 * (avg_price / avg_sales)

t_stat <- (elasticity_hat - null_elasticity) / se_elasticity

p_val <- 2 * pt(abs(t_stat), df = df.residual(model_tuna), lower.tail = FALSE)

t_crit_10 <- qt(0.95, df = df.residual(model_tuna))

cat("\n--- (f) Hypothesis Test ---\n")
cat("t 統計量:", t_stat, "\n")
cat("p-value:", p_val, "\n")
cat("10% 顯著水準下的 t 臨界值: +/-", t_crit_10, "\n")

if(p_val < 0.10) {
  cat("結論：拒絕 H0，彈性顯著不等於 -3\n")
} else {
  cat("結論：不拒絕 H0，沒有足夠證據說明彈性與 -3 有顯著差異\n")
}