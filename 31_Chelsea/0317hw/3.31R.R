library("POE5Rdata")
data('tuna',package='POE5Rdata')
summary(tuna[c('sal1','apr1')])
plot(tuna$sal1,type='l',col='blue',main='weekly tuna sale',xlab='week',ylab='sales')
plot(tuna$apr1,type='l',col='red',main='weekly tuna price',xlab='week',ylab='price')
plot(tuna$apr1, tuna$sal1, pch=19, col="darkgreen",
     main="Sales vs Price", xlab="Price (APR1)", ylab="Sales (SAL1)")
# --- (c) 單位換算與線性迴歸 ---
# 創造新變數：把美金換成美分 (1 dollar = 100 cents)
tuna$price1 <- 100 * tuna$apr1

# 估計迴歸模型：SAL1 = b1 + b2*PRICE1 + e
model_tuna <- lm(sal1 ~ price1, data = tuna)
summary(model_tuna)

# 找出 95% 信賴區間（針對價格每變動 1 美分對銷售的影響，即 b2）
confint(model_tuna, "price1", level = 0.95)

# --- (d) 點預測：當價格為 70 美分時 ---
new_price <- data.frame(price1 = 70)
pred_d <- predict(model_tuna, new_price, interval = "confidence", level = 0.90)
cat("當價格為 70 美分時，預期銷售量區間：\n")
print(pred_d)

# --- (e) 價格彈性 (Elasticity) ---
# 公式：Elasticity = b2 * (Price_mean / Sales_mean)
b2 <- coef(model_tuna)["price1"]
mean_p1 <- mean(tuna$price1)
mean_s1 <- mean(tuna$sal1)

elasticity <- b2 * (mean_p1 / mean_s1)
cat("在平均值處的價格彈性為:", elasticity, "\n")

# --- (f) 假設檢定：測試彈性是否等於 -3 ---
# H0: b2 * (mean_p1 / mean_s1) = -3  =>  b2 = -3 * (mean_s1 / mean_p1)
hyp_b2 <- -3 * (mean_s1 / mean_p1)
se_b2 <- summary(model_tuna)$coefficients["price1", "Std. Error"]
t_stat_f <- (b2 - hyp_b2) / se_b2
p_val_f <- 2 * (1 - pt(abs(t_stat_f), df = df.residual(model_tuna))) # 雙尾
cat("測試彈性是否為 -3 的 t 統計量:", t_stat_f, " P值:", p_val_f)
