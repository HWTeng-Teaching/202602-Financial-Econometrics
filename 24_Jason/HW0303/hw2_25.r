load("cex5_small.rdata")
# a. 敘述性統計與直方圖
hist(cex5_small$foodaway, main="Histogram of FOODAWAY", xlab="Food Away ($)", col="lightblue")
summary(cex5_small$foodaway)
# 額外計算百分位數
quantile(cex5_small$foodaway, probs = c(0.25, 0.75))

# b. 依教育程度分類統計
# 計算 Advanced Degree 
summary(subset(cex5_small, advanced == 1)$foodaway)
# 計算 College Degree
summary(subset(cex5_small, college == 1)$foodaway)
# 計算 No Degree
summary(subset(cex5_small, advanced == 0 & college == 0)$foodaway)

# c. 對數轉換與直方圖
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
hist(cex5_small$ln_foodaway, main="Histogram of ln(FOODAWAY)", xlab="ln(Food Away)", col="lightgreen")
df_positive <- subset(cex5_small, foodaway > 0)
df_positive$ln_foodaway <- log(df_positive$foodaway)
summary(df_positive$ln_foodaway)
sd(df_positive$ln_foodaway)

# d. 線性回歸估計
df_clean <- subset(cex5_small, foodaway > 0)
model_d <- lm(log(foodaway) ~ income, data = df_clean)
summary(model_d)

# e. 繪製回歸線圖
plot(cex5_small$income, cex5_small$ln_foodaway, pch=20, col="gray",
     main="ln(FOODAWAY) vs. INCOME", xlab="Income ($100 units)", ylab="ln(Food Away)")
abline(model_d, col="red", lwd=2)

# f. 殘差分析
cex5_small_clean <- subset(cex5_small, foodaway > 0)
cex5_small_clean$residuals <- residuals(model_d)
plot(cex5_small_clean$income, cex5_small_clean$residuals, pch=20, 
     main="Residuals vs. Income", xlab="Income", ylab="Residuals")
abline(h=0, lty=2, col="blue")