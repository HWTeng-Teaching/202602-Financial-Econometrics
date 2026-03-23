n <- 20                  # 樣本數
df <- n - 2              # 自由度
beta1_hat <- 6.855       # 截距估計值
beta2_hat <- 3.880       # 斜率估計值
se_beta1 <- 7.383        # 截距標準誤
se_beta2 <- 0.112        # 斜率標準誤

mean_income <- 59.3      # INCOME 樣本平均數

# ==========================================
# (a) 視覺化：迴歸線草圖與樣本平均數點
# ==========================================
# 計算 INSURANCE 樣本平均數
mean_insurance <- beta1_hat + beta2_hat * mean_income
cat("(a) 樣本平均數點座標為: (", mean_income, ",", mean_insurance, ")\n\n")

# 建立空白畫布，設定 XY 軸範圍 (收入從 0 到 100，保險從 0 到 450)
plot(0, 0, type="n", xlim=c(0, 100), ylim=c(0, 450),
     xlab="Household Income (Thousands of $)",
     ylab="Life Insurance (Thousands of $)",
     main="Fitted Relationship: Insurance vs Income")

# 畫出估計的迴歸線
abline(a=beta1_hat, b=beta2_hat, col="blue", lwd=2)

# 標示 Y 軸截距
points(0, beta1_hat, col="darkorange", pch=19, cex=1.5)
text(0, beta1_hat, paste("Intercept\n", beta1_hat), pos=4, col="darkorange")

# 標示樣本平均數點 (X_bar, Y_bar)
points(mean_income, mean_insurance, col="red", pch=19, cex=1.5)
text(mean_income, mean_insurance, "Sample Mean", pos=2, col="red")