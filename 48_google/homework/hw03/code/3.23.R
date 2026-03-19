# 清除記憶體
rm(list=ls())

# 載入 POE5Rdata 套件
library(POE5Rdata)

# 呼叫特定數據集
data("collegetown")

# 顯示前幾行數據
head(collegetown)

# 顯示數據統計摘要
summary(collegetown)

# ==========================================
# 建立二次回歸模型: price = a1 + a2 * sqft^2 + e
# ==========================================
# 注意：根據妳的數據摘要，欄位名為小寫
model_house <- lm(price ~ I(sqft^2), data = collegetown)
model_summary <- summary(model_house)

# 提取參數估計值與標準誤差
ALPHA2_HAT <- coef(model_house)["I(sqft^2)"]
SE_ALPHA2 <- model_summary$coefficients["I(sqft^2)", "Std. Error"]
DF_DEGREES <- df.residual(model_house)

# --- (a) 2,000 平方英尺 (sqft = 20) 的邊際效應檢定 ---
# 邊際效應 ME = 2 * a2 * sqft = 40 * a2
# H0: 40 * a2 <= 13 vs H1: 40 * a2 > 13
me_20 <- 40 * ALPHA2_HAT
se_me_20 <- 40 * SE_ALPHA2
t_stat_a <- (me_20 - 13) / se_me_20
p_value_a <- pt(t_stat_a, DF_DEGREES, lower.tail = FALSE)

# --- (b) 4,000 平方英尺 (sqft = 40) 的邊際效應檢定 ---
# 當 sqft = 40, ME = 2 * a2 * 40 = 80 * a2
# H0: 80 * a2 <= 13 vs H1: 80 * a2 > 13
me_40 <- 80 * ALPHA2_HAT
se_me_40 <- 80 * SE_ALPHA2
t_stat_b <- (me_40 - 13) / se_me_40
p_value_b <- pt(t_stat_b, DF_DEGREES, lower.tail = FALSE)

# --- (c) sqft = 20 的預期價格與 95% 信賴區間 ---
new_house_20 <- data.frame(sqft = 20)
conf_interval_c <- predict(model_house, 
                           newdata = new_house_20, 
                           interval = "confidence", 
                           level = 0.95)

# --- (d) 樣本中 sqft = 20 的實際平均售價 ---
# 尋找所有面積剛好為 20 (2,000 sq ft) 的樣本
actual_houses_20 <- collegetown[collegetown$sqft == 20, ]
sample_mean_20 <- mean(actual_houses_20$price)

# ==========================================
# 輸出分析結果
# ==========================================
cat("\n--- 題目 3.23 統計報告 ---\n")
cat("(a) 2000 sqft 邊際效應:", me_20, "\n    t 統計量:", t_stat_a, " p-value:", p_value_a, "\n")
cat("(b) 4000 sqft 邊際效應:", me_40, "\n    t 統計量:", t_stat_b, " p-value:", p_value_b, "\n")
cat("(c) 2000 sqft 預期價格估計值:", conf_interval_c[1], "\n")
cat("    95% 信賴區間: [", conf_interval_c[2], ",", conf_interval_c[3], "]\n")
cat("(d) 樣本中 sqft=20 的觀測數量:", nrow(actual_houses_20), "\n")
cat("    樣本平均售價:", sample_mean_20, "\n")