url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
dest_file <- tempfile(fileext = ".rdata") # 副檔名
download.file(url, destfile = dest_file, mode = "wb")
load(dest_file)

df <- collegetown # 資料名稱統一為 df 
rm(collegetown) #remove

# 載入檢定用套件
if (!require("tseries")) install.packages("tseries") #time series
library(tseries)

# --- (a) Log-Linear Model ---
# ln(PRICE) = b1 + b2*SQFT + e
model_a <- lm(log(price) ~ sqft, data = df)
cat("\n[a] Log-Linear Model Summary:\n")
print(summary(model_a))
# 彈性 (Elasticity at means): beta2 * mean(sqft)
elasticity_a <- coef(model_a)["sqft"] * mean(df$sqft)
cat("Elasticity at mean (Log-Linear):", elasticity_a, "\n")

# --- (b) Log-Log Model ---
# ln(PRICE) = a1 + a2*ln(SQFT) + e
model_b <- lm(log(price) ~ log(sqft), data = df)
cat("\n[b] Log-Log Model Summary:\n")
print(summary(model_b))
# 彈性 (Elasticity): 在 Log-Log 中即為斜率係數
elasticity_b <- coef(model_b)["log(sqft)"]
cat("Elasticity (Log-Log):", elasticity_b, "\n")

# --- (c) Linear Model & Generalized R-squared ---
model_linear <- lm(price ~ sqft, data = df)
# 計算 Generalized R-squared (用原始單位的 y 與預測值的相關係數平方)
gen_r2_a <- cor(df$price, exp(predict(model_a)))^2  #(r_y,yhat)^2=R^2
gen_r2_b <- cor(df$price, exp(predict(model_b)))^2
cat("\n[c] R-squared Comparison:\n")
cat("Linear R2:", summary(model_linear)$r.squared, "\n")
cat("Log-Linear Generalized R2:", gen_r2_a, "\n")
cat("Log-Log Generalized R2:", gen_r2_b, "\n")

# --- (d) Residual Histograms & Jarque-Bera Test ---
par(mfrow=c(1,3)) # 設定畫布 
hist(residuals(model_a), main="Resid: Log-Linear", col="lightblue")
hist(residuals(model_b), main="Resid: Log-Log", col="lightgreen")
hist(residuals(model_linear), main="Resid: Linear", col="pink")


# 模型誤差是否符合常態分佈(Jarque Bera Test) 
# 虛無假設是符合常態，故P-Value>alpha時不可拒絕虛無
jb_a <- jarque.bera.test(residuals(model_a))
jb_b <- jarque.bera.test(residuals(model_b))
jb_linear <- jarque.bera.test(residuals(model_linear))
cat("\n[d] Jarque-Bera Test p-values:\n")
cat("Log-Linear:", jb_a$p.value, "| Log-Log:", jb_b$p.value, "| Linear:", jb_linear$p.value, "\n")

# --- (e) Plot Residuals vs SQFT ---
# 設定畫布：1列3欄
par(mfrow=c(1,3))

# 圖 1: Linear Model
plot(df$sqft, residuals(model_linear), 
     main="Linear: Residuals vs SQFT", 
     xlab="SQFT", ylab="Residuals", col="gray")
abline(h=0, col="red", lwd=2) #水平線在0處

# 圖 2: Log-Linear Model
plot(df$sqft, residuals(model_a), 
     main="Log-Linear: Residuals vs SQFT", 
     xlab="SQFT", ylab="Residuals", col="blue")
abline(h=0, col="red", lwd=2)

# 圖 3: Log-Log Model
plot(df$sqft, residuals(model_b), 
     main="Log-Log: Residuals vs SQFT", 
     xlab="SQFT", ylab="Residuals", col="darkgreen")
abline(h=0, col="red", lwd=2)
# 觀察是否有漏斗狀，若有則代表異質變異

par(mfrow=c(1,1)) # 畫完後重設畫布 Multi-Frame, Row-wise

#(f&g)
# 1. 建立預測資料 (2700 呎 = 27 單位)
new_house <- data.frame(sqft = 27)

# --- 模型 (c): Linear Model (線性模型) ---
# 因為 y 是 Price，直接預測即可
pred_linear <- predict(model_linear, new_house, interval = "prediction")
cat("\n[Model C - Linear] 預測值與區間:\n")
print(pred_linear)

# --- 模型 (a): Log-Linear Model (半對數模型) ---
# 因為 y 是 log(Price)，需要用 exp() 還原
pred_a_log <- predict(model_a, new_house, interval = "prediction")
pred_a_price <- exp(pred_a_log)
cat("\n[Model A - Log-Linear] 預測值與區間 (已還原):\n")
print(pred_a_price)

# --- 模型 (b): Log-Log Model (雙對數模型) ---
# y 是 log(Price)，用 exp() 還原
pred_b_log <- predict(model_b, new_house, interval = "prediction")
pred_b_price <- exp(pred_b_log)
cat("\n[Model B - Log-Log] 預測值與區間 (已還原):\n")
print(pred_b_price)