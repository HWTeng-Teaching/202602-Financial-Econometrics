# collegetown 資料集載入
load("collegetown.rdata")

# 1. 建立平方項變數
collegetown$sqft2 <- collegetown$sqft^2

# 2. 估計二次回歸模型
model_q <- lm(price ~ sqft2, data=collegetown)

# 提取參數與標準誤
alpha2_hat <- coef(model_q)["sqft2"]
se_alpha2 <- summary(model_q)$coefficients["sqft2", "Std. Error"]
df <- df.residual(model_q)

# --- (a) 檢定 2000 sq ft (SQFT = 20) ---
# 邊際效果 = 2 * alpha2 * 20 = 40 * alpha2
me_a <- 40 * alpha2_hat
se_me_a <- 40 * se_alpha2 # 標準誤也要乘 40
t_stat_a <- (me_a - 13) / se_me_a
p_val_a <- pt(t_stat_a, df, lower.tail = FALSE) # 這是「大於」的單尾檢定 (右尾)

cat("(a) t-statistic:", t_stat_a, " p-value:", p_val_a, "\n")

# --- (b) 檢定 4000 sq ft (SQFT = 40) ---
# 邊際效果 = 2 * alpha2 * 40 = 80 * alpha2
me_b <- 80 * alpha2_hat
se_me_b <- 80 * se_alpha2 # 標準誤也要乘 80
t_stat_b <- (me_b - 13) / se_me_b
p_val_b <- pt(t_stat_b, df, lower.tail = FALSE)

cat("(b) t-statistic:", t_stat_b, " p-value:", p_val_b, "\n")

# --- (c) 估計 2000 sq ft 的預期價格與 95% 信賴區間 ---
new_house <- data.frame(sqft2 = 20^2) # 記得帶入的是平方後的值
pred_c <- predict(model_q, newdata=new_house, interval="confidence", level=0.95)
print(pred_c)

# --- (d) 比較樣本平均 ---
sample_houses <- subset(collegetown, sqft == 20)
mean_price_d <- mean(sample_houses$price)
cat("(d) Sample mean price for SQFT=20:", mean_price_d, "\n")