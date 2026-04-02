# ==========================================
# 0. 環境準備與資料載入
# ==========================================
rm(list=ls()) # 清空環境

# 確保安裝並載入 tseries 套件 (為了算 (d) 小題的 Jarque-Bera 檢定)
if(!require(tseries)) {install.packages("tseries"); library(tseries)}

# 網址
data_url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata" 
load(url(data_url))

# 建立取對數的變數
collegetown$ln_price <- log(collegetown$price)
collegetown$ln_sqft <- log(collegetown$sqft)

# ==========================================
# (a) Log-Linear Model: ln(PRICE) = B1 + B2*SQFT
# ==========================================
model_A <- lm(ln_price ~ sqft, data = collegetown)
summary(model_A)

# 【解釋模型參數】：
# B2 的估計值代表：當房屋面積 (SQFT) 增加 1 單位，房屋價格 (PRICE) 預期會改變 (B2 * 100)%。

# 計算樣本平均下的 Slope 與 Elasticity
x_mean <- mean(collegetown$sqft)
pred_lny_A <- predict(model_A, newdata = data.frame(sqft = x_mean))
y_hat_A <- exp(pred_lny_A) # 轉回 PRICE

slope_A <- coef(model_A)["sqft"] * y_hat_A
elas_A <- coef(model_A)["sqft"] * x_mean

cat("\n--- (a) Log-Linear 結果 ---\n")
cat("Slope at mean:", slope_A, "\nElasticity at mean:", elas_A, "\n")

# ==========================================
# (b) Log-Log Model: ln(PRICE) = a1 + a2*ln(SQFT)
# ==========================================
model_B <- lm(ln_price ~ ln_sqft, data = collegetown)
summary(model_B)

# 【解釋模型參數】：
# a2 的估計值即為「彈性」(Elasticity)，代表：當房屋面積 (SQFT) 增加 1%，房屋價格預期增加 a2%。

pred_lny_B <- predict(model_B, newdata = data.frame(ln_sqft = log(x_mean)))
y_hat_B <- exp(pred_lny_B)

elas_B <- coef(model_B)["ln_sqft"] # Log-log 模型的斜率係數直接就是彈性
slope_B <- elas_B * (y_hat_B / x_mean)

cat("\n--- (b) Log-Log 結果 ---\n")
cat("Slope at mean:", slope_B, "\nElasticity at mean:", elas_B, "\n")

# ==========================================
# (c) Linear Model 與 Generalized R^2
# ==========================================
# 建立線性模型 PRICE = d1 + d2*SQFT
model_C <- lm(price ~ sqft, data = collegetown)

# Linear 模型的 R^2
R2_linear <- summary(model_C)$r.squared

# 計算 Log-Linear (A) 與 Log-Log (B) 的 Generalized R^2
# 公式：[Corr(實際 Y, 預測 Y)]^2
y_actual <- collegetown$price

gen_R2_A <- cor(y_actual, exp(predict(model_A)))^2
gen_R2_B <- cor(y_actual, exp(predict(model_B)))^2

cat("\n--- (c) R-squared 比較 ---\n")
cat("Linear R2:", R2_linear, "\nGen R2 (Log-Lin):", gen_R2_A, "\nGen R2 (Log-Log):", gen_R2_B, "\n")

# ==========================================
# (d) 殘差直方圖與 Jarque-Bera 檢定
# ==========================================
res_A <- residuals(model_A)
res_B <- residuals(model_B)
res_C <- residuals(model_C)

# 畫出三個模型的殘差直方圖 (設定畫布為 1 列 3 欄)
par(mfrow=c(1,3))
hist(res_A, main="Log-Lin Residuals", xlab="Residuals", col="lightblue")
hist(res_B, main="Log-Log Residuals", xlab="Residuals", col="lightgreen")
hist(res_C, main="Linear Residuals", xlab="Residuals", col="lightcoral")

# Jarque-Bera Test
jb_A <- jarque.bera.test(res_A)
jb_B <- jarque.bera.test(res_B)
jb_C <- jarque.bera.test(res_C)

cat("\n--- (d) Jarque-Bera p-values (H0: 常態分配) ---\n")
cat("Log-Lin:", jb_A$p.value, "\nLog-Log:", jb_B$p.value, "\nLinear:", jb_C$p.value, "\n")
# 【結論判斷】：如果 p-value < 0.05，代表拒絕常態分配的假設。

# ==========================================
# (e) 殘差 vs SQFT 散佈圖 (檢查異質變異或模式)
# ==========================================
par(mfrow=c(1,3))
plot(collegetown$sqft, res_A, main="Log-Lin: Res vs SQFT", ylab="Residuals", xlab="SQFT")
abline(h=0, col="red")
plot(collegetown$sqft, res_B, main="Log-Log: Res vs SQFT", ylab="Residuals", xlab="SQFT")
abline(h=0, col="red")
plot(collegetown$sqft, res_C, main="Linear: Res vs SQFT", ylab="Residuals", xlab="SQFT")
abline(h=0, col="red")
# 【結論判斷】：觀察點的散佈是否隨 SQFT 變大而呈扇形展開 (這代表有異質變異 Heteroskedasticity)。通常 Linear 模型會有明顯的扇形。

# ==========================================
# (f) & (g) 預測 SQFT = 2700 的房價與 95% 預測區間
# ==========================================
new_house <- data.frame(sqft = 2700, ln_sqft = log(2700))

# 取得 95% Prediction Intervals (預測區間)
pred_A_log <- predict(model_A, newdata = new_house, interval = "prediction", level = 0.95)
pred_B_log <- predict(model_B, newdata = new_house, interval = "prediction", level = 0.95)
pred_C <- predict(model_C, newdata = new_house, interval = "prediction", level = 0.95)

# 將 Log 模型的結果用 exp() 轉回原本的 Price 單位
pred_A_price <- exp(pred_A_log)
pred_B_price <- exp(pred_B_log)

cat("\n--- (f) & (g) Predictions and 95% PI for SQFT=2700 ---\n")
print(data.frame(Model = c("Log-Lin", "Log-Log", "Linear"),
                 Fit_Price = c(pred_A_price[1], pred_B_price[1], pred_C[1]),
                 Lower_95 = c(pred_A_price[2], pred_B_price[2], pred_C[2]),
                 Upper_95 = c(pred_A_price[3], pred_B_price[3], pred_C[3])))

# 還原畫布設定
par(mfrow=c(1,1))

# ==========================================
# (h) 結論：你會選擇哪個模型？ (文字回答參考)
# ==========================================
# 參考寫法：
# 綜合比較下，Log-Log 或 Log-Lin 模型通常優於單純的 Linear 模型。
# 1. Generalized R^2：對數模型通常能提供較高的解釋力。
# 2. 殘差圖 (e)：對數轉換 (taking logs) 通常能有效減輕「異質變異」(heteroskedasticity) 的問題，讓殘差分佈更平均。
# 3. 殘差分配 (d)：看 JB test 的結果，哪個模型的 p-value 越不顯著（或者直方圖越對稱），越符合線性迴歸的基本假設。