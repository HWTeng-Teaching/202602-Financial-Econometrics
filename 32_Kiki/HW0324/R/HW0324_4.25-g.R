# 確保環境乾淨
rm(list=ls()) 

# 載入資料
data_url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata" 
load(url(data_url))


sqft = collegetown$sqft
price = collegetown$price
ln_price = log(price)
ln_sqft = log(sqft)

mean_sqft = mean(sqft)
mean_ln_sqft = mean(ln_sqft) 


model_1 = lm(ln_price ~ sqft , data = collegetown)
model_2 = lm(ln_price ~ ln_sqft , data = collegetown)
model_3 = lm(price ~ sqft , data = collegetown)


b1 = coefficients(model_1)[1]
b2 = coefficients(model_1)[2]
a1 = coefficients(model_2)[1]
a2 = coefficients(model_2)[2]
d1 = coefficients(model_3)[1]
d2 = coefficients(model_3)[2]

# 計算點預測值
price_1_hat = b1 + 27*b2
price_2_hat = a1 + log(27)*a2
price_3_hat = d1 + 27*d2


N = 500
tvalue = qt(0.975, N - 2) # df = 498

# ===== Model 1: Log-Linear =====
vara_1_2 = vcov(model_1)[2,2]
sm1 = summary(model_1)
sigma_hat_1 = sm1$sigma^2 
varf_1 = sigma_hat_1 + sigma_hat_1/N + (27 - mean_sqft)^2 * vara_1_2 
sef_1 = sqrt(varf_1)
lowb_1 = exp(as.numeric(price_1_hat) - tvalue * sef_1)
upb_1 = exp(as.numeric(price_1_hat) + tvalue * sef_1)

# ===== Model 2: Log-Log =====
vara_2_2 = vcov(model_2)[2,2]
sm2 = summary(model_2)
sigma_hat_2 = sm2$sigma^2 

varf_2 = sigma_hat_2 + sigma_hat_2/N + (log(27) - mean_ln_sqft)^2 * vara_2_2 
sef_2 = sqrt(varf_2)
lowb_2 = exp(as.numeric(price_2_hat) - tvalue * sef_2)
upb_2 = exp(as.numeric(price_2_hat) + tvalue * sef_2)

# ===== Model 3: Linear =====
vara_3_2 = vcov(model_3)[2,2]
sm3 = summary(model_3)
sigma_hat_3 = sm3$sigma^2
varf_3 = sigma_hat_3 + sigma_hat_3/N + (27 - mean_sqft)^2 * vara_3_2
sef_3 = sqrt(varf_3)
lowb_3 = as.numeric(price_3_hat) - tvalue * sef_3
upb_3 = as.numeric(price_3_hat) + tvalue * sef_3


cat("\n--- 預測面積 2700 sqft (X=27) 的 95% 房價預測區間 ---\n")
cat(sprintf("Model 1 (Log-Linear): [%.4f, %.4f]\n", lowb_1, upb_1))
cat(sprintf("Model 2 (Log-Log)   : [%.4f, %.4f]\n", lowb_2, upb_2))
cat(sprintf("Model 3 (Linear)    : [%.4f, %.4f]\n", lowb_3, upb_3))