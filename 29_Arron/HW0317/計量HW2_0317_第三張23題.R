url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
dest_file <- "C:/Users/user/Desktop/college.rdata"
download.file(url, destfile = dest_file, mode = "wb")
load(dest_file)
ls()
# 1. 建立模型 
model_final <- lm(price ~ I(sqft^2), data = collegetown)
sum_res <- summary(model_final)

# 2. 提取統計基礎資料
a2_hat <- sum_res$coefficients["I(sqft^2)", "Estimate"]
se_a2 <- sum_res$coefficients["I(sqft^2)", "Std. Error"]
df_val <- df.residual(model_final)
alpha <- 0.05

# 3. 設定臨界值與拒絕域 (右尾檢定)
critical_val <- qt(1 - alpha, df = df_val)

# --- (a) 2000 sqft (sqft=20) 的邊際效應檢定 ---
# H0: 40*a2 <= 13  vs  H1: 40*a2 > 13
me_20 <- 40 * a2_hat
se_me_20 <- 40 * se_a2
t_a <- (me_20 - 13) / se_me_20
p_a <- pt(t_a, df = df_val, lower.tail = FALSE)

# --- (b) 4000 sqft (sqft=40) 的邊際效應檢定 ---
# H0: 80*a2 <= 13  vs  H1: 80*a2 > 13
me_40 <- 80 * a2_hat
se_me_40 <- 80 * se_a2
t_b <- (me_40 - 13) / se_me_40
p_b <- pt(t_b, df = df_val, lower.tail = FALSE)

# --- (c) 預期價格與 95% 信賴區間 (sqft=20) ---
new_h <- data.frame(sqft = 20)
pred_c <- predict(model_final, newdata = new_h, interval = "confidence", level = 0.95)

# --- (d) 樣本中 sqft=20 的平均價格 ---
avg_p_20 <- mean(collegetown$price[collegetown$sqft == 20], na.rm = TRUE)

# ==================== 輸出結果報告 ====================
cat("【統計基準】\n")
cat("臨界值 (Critical Value):", critical_val, "\n")
cat("拒絕域 (Rejection Region): t >", critical_val, "\n\n")

cat("【(a) 小題：2000 sqft】\n")
cat("邊際效應:", me_20, "\nt 統計量:", t_a, "\n右尾 p-value:", p_a, "\n")
cat("結論:", ifelse(t_a > critical_val, "拒絕 H0", "無法拒絕 H0"), "\n\n")

cat("【(b) 小題：4000 sqft】\n")
cat("邊際效應:", me_40, "\nt 統計量:", t_b, "\n右尾 p-value:", p_b, "\n")
cat("結論:", ifelse(t_b > critical_val, "拒絕 H0", "無法拒絕 H0"), "\n\n")

cat("【(c) 小題：預測區間】\n")
cat("預期價格:", pred_c[1, "fit"], "\n95% CI: [", pred_c[1, "lwr"], ",", pred_c[1, "upr"], "]\n\n")

cat("【(d) 小題：樣本平均】\n")
cat("2000 sqft 真實平均房價:", avg_p_20, "\n")