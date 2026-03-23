#31

#a
# 將所有變數名稱轉為大寫，符合題目的寫法 (SAL1, APR1)
colnames(tuna_data) <- toupper(colnames(tuna_data))
# 資料本身沒有 WEEK 欄位，手動新增 1 到 52 週的變數
tuna_data$WEEK <- 1:nrow(tuna_data)
cat("\n--- (a) 敘述統計 ---\n")
# 計算 SAL1 和 APR1 的基本統計量 (包含平均數、最小、最大)
print(summary(tuna_data[c("SAL1", "APR1")]))
# 計算標準差
sd(tuna_data$SAL1)
sd(tuna_data$APR1)
#畫折線圖
plot(tuna_data$WEEK, tuna_data$SAL1, type="l", col="blue", 
     main="Weekly Sales (SAL1)", xlab="Week", ylab="Sales")
plot(tuna_data$WEEK, tuna_data$APR1, type="l", col="red", 
     main="Weekly Price (APR1)", xlab="Week", ylab="Price ($)")

#b
# 畫散佈圖 (X軸為價格 APR1，Y軸為銷量 SAL1)
plot(tuna_data$APR1, tuna_data$SAL1, pch=16, col="darkgreen",
     main="Sales vs Price", xlab="Price (APR1)", ylab="Sales (SAL1)")

#c
tuna_data$PRICE1 <- 100 * tuna_data$APR1
model<- lm(SAL1~PRICE1, data=tuna_data)
summary(model)
print(confint(model, level=0.95))

#d
new_data <- data.frame(PRICE1 = 70)
predict_ci <- predict(model, newdata=new_data, interval="confidence", level=0.90)
print(predict_ci)

#e
mean_P <- mean(tuna_data$PRICE1)
mean_S <- mean(tuna_data$SAL1)
elasticity <- beta2_hat * (mean_P / mean_S)
se_elasticity <- se_beta2 * (mean_P / mean_S)
df <- df.residual(model)
tcr <- qt(0.975, df)
lower_E <- elasticity - tcr * se_elasticity
upper_E <- elasticity + tcr * se_elasticity

#f
alpha<- 0.1
elasticity <- beta2_hat * (mean_P / mean_S)
se_elasticity <- se_beta2 * (mean_P / mean_S)
df<- df.residual(model)
t<- (elasticity-(-3))/se_elasticity
tcr<- qt(1-(alpha/2), df)
p <- 2*(1-pt(abs(t), df)) 
