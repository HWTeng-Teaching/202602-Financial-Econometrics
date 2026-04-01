rm(list=ls())

# 載入必要套件 (新增 sandwich 與 lmtest 用於穩健標準誤)
if(!require(POE5Rdata)) install.packages("POE5Rdata")
if(!require(tseries)) install.packages("tseries")
if(!require(stargazer)) install.packages("stargazer")
if(!require(sandwich)) install.packages("sandwich")
if(!require(lmtest)) install.packages("lmtest")

library(POE5Rdata)
library(tseries)
library(stargazer)
library(sandwich)
library(lmtest)

# 1. 數據載入
data("collegetown")
df <- collegetown

# 2. 模型估計
mod_linear <- lm(price ~ sqft, data = df)
mod_loglinear <- lm(log(price) ~ sqft, data = df)
mod_loglog <- lm(log(price) ~ log(sqft), data = df)

# 3. 計算穩健標準誤 (Robust Standard Errors - HC1)
cov_linear <- vcovHC(mod_linear, type = "HC1")
robust_se_linear <- sqrt(diag(cov_linear))

cov_loglinear <- vcovHC(mod_loglinear, type = "HC1")
robust_se_loglinear <- sqrt(diag(cov_loglinear))

cov_loglog <- vcovHC(mod_loglog, type = "HC1")
robust_se_loglog <- sqrt(diag(cov_loglog))

# --- 輸出敘述性統計表 ---
print("--- 敘述性統計 (Descriptive Statistics) ---")
stargazer(df[, c("price", "sqft")], type = "text", 
          title="Descriptive Statistics", digits=2)

# --- 輸出穩健迴歸對照表 ---
print("--- 穩健標準誤迴歸模型對照表 (Robust SE) ---")
stargazer(mod_linear, mod_loglinear, mod_loglog, 
          type = "text", 
          se = list(robust_se_linear, robust_se_loglinear, robust_se_loglog),
          column.labels = c("Linear", "Log-Linear", "Log-Log"),
          digits = 4)

# 4. 預測與「偏誤修正 (Retransformation Bias Correction)」
x_new <- data.frame(sqft = 27)

# Linear 預測 (無需修正)
pred_lin <- predict(mod_linear, x_new)

# Log-Linear 預測與修正
sigma_loglin <- summary(mod_loglinear)$sigma
pred_loglin_raw <- predict(mod_loglinear, x_new)
# 修正公式：E[y|x] = exp(ln_y + sigma^2 / 2)
pred_loglin_corrected <- exp(pred_loglin_raw + (sigma_loglin^2)/2)

# Log-Log 預測與修正
sigma_loglog <- summary(mod_loglog)$sigma
pred_loglog_raw <- predict(mod_loglog, x_new)
pred_loglog_corrected <- exp(pred_loglog_raw + (sigma_loglog^2)/2)

print("--- SQFT = 27 的房價預測 (加入變異數修正) ---")
print(data.frame(
  Model = c("Linear", "Log-Linear", "Log-Log"),
  Uncorrected_Forecast = c(pred_lin, exp(pred_loglin_raw), exp(pred_loglog_raw)),
  Corrected_Forecast = c(pred_lin, pred_loglin_corrected, pred_loglog_corrected)
))