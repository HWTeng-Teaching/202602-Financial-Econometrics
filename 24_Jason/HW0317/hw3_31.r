load("tuna.rdata")

# 設定資料 (假設你已經載入 tuna.rdata)
tuna$week <- 1:52

# (a) 敘述統計
summary(tuna$sal1)
sd(tuna$sal1)
summary(tuna$apr1)
sd(tuna$apr1)

# 畫圖 (a)
par(mfrow=c(2,1)) # 把兩張圖上下並排
plot(tuna$week, tuna$sal1, type="l", col="blue", main="Weekly Sales (SAL1)", xlab="Week", ylab="Sales")
plot(tuna$week, tuna$apr1, type="l", col="red", main="Weekly Price (APR1)", xlab="Week", ylab="Price ($)")
par(mfrow=c(1,1)) # 恢復單圖模式

# (b) 散佈圖
plot(tuna$apr1, tuna$sal1, pch=20, main="Sales vs. Price", xlab="Price ($)", ylab="Sales")

# (c) 線性回歸與信賴區間
tuna$price1 <- 100 * tuna$apr1 # 轉換成美分 (cents)
model_c <- lm(sal1 ~ price1, data=tuna)
summary(model_c)
confint(model_c, "price1", level=0.95) # 95% CI

# (d) 預測當 price = 70 cents 時的預期銷售量 (90% CI)
new_p <- data.frame(price1 = 70)
predict(model_c, newdata=new_p, interval="confidence", level=0.90)

# (e) 計算平均值處的彈性 (Elasticity at the means) 與 95% CI
mean_p <- mean(tuna$price1)
mean_q <- mean(tuna$sal1)
ratio <- mean_p / mean_q

beta2 <- coef(model_c)["price1"]
elasticity <- beta2 * ratio

ci_beta2 <- confint(model_c, "price1", level=0.95)
ci_elasticity <- ci_beta2 * ratio

cat("Point Elasticity:", elasticity, "\n")
cat("95% CI for Elasticity:", ci_elasticity, "\n")

# (f) 假設檢定 H0: Elasticity = -3, alpha = 0.10
# 數學轉換：檢定 Elasticity = -3 等同於檢定 beta2 = -3 / ratio
alpha <- 0.10
df <- df.residual(model_c)

se_beta2 <- summary(model_c)$coefficients["price1", "Std. Error"]
se_elasticity <- se_beta2 * ratio

t_stat <- (elasticity - (-3)) / se_elasticity
p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
t_crit <- qt(1 - alpha/2, df) # 雙尾檢定的臨界值

cat("t-statistic:", t_stat, "\n")
cat("Critical value:", t_crit, "\n")
cat("p-value:", p_val, "\n")