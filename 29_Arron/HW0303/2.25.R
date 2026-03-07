load("C:/Users/user/Desktop/cex5_small.rdata")


# --- (a) 直方圖與敘述統計 ---
cat("\n--- (a) 小題結果 ---\n")
# 將 foodaway 強制轉為數值格式
cex5_small$foodaway <- as.numeric(cex5_small$foodaway)
hist(cex5_small$foodaway, main="Histogram of foodaway", xlab="Expenditure")
summary(cex5_small$foodaway)
quantile(cex5_small$foodaway,probs=c(0.25,0.75))

# --- (b) 按學歷分組的平均值與中位數 ---
cat("\n--- (b) 小題結果 ---\n")
cat("高級學位 (Advanced):")
summary(subset(cex5_small, advanced == 1)$foodaway)[c("Mean", "Median")]
cat("大學學位 (College):")
summary(subset(cex5_small, college == 1)$foodaway)[c("Mean", "Median")]
cat("無上述學位:")
summary(subset(cex5_small, advanced == 0 & college == 0)$foodaway)[c("Mean", "Median")]

# --- (c) 對數轉換與直方圖 ---
cat("\n--- (c) 小題結果 ---\n")
# 只有 foodaway > 0 才能取對數，並排除 NA
data_c <- subset(cex5_small, foodaway > 0 & !is.na(foodaway))
data_c$ln_foodaway <- log(data_c$foodaway)
hist(data_c$ln_foodaway, main="Histogram of ln(foodaway)", xlab="ln(Expenditure)")
summary(data_c$ln_foodaway)
n_a <- nrow(cex5_small)
cat("Number of observation in foodaway:", n_a, "\n")
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
n_c <- sum(is.finite(cex5_small$ln_foodaway))
cat("Number of observation in ln(foodaway):", n_c, "\n")

# --- (d) 線性回歸估計 ---
cat("\n--- (d) 小題結果 ---\n")
# 確保 income 也是數值
data_c$income <- as.numeric(data_c$income)
model_d <- lm(ln_foodaway ~ income, data = data_c)
coef(model_d)

# --- (e) 繪製散佈圖與回歸線 ---
plot(data_c$income, data_c$ln_foodaway, main="ln(foodaway) vs income")
abline(model_d, col="red")

# --- (f) 殘差分析與繪圖 ---
cat("\n--- (f) 小題結果 ---\n")
data_c$e_hat <- residuals(model_d)
plot(data_c$income, data_c$e_hat, main="Residuals vs income")
abline(h=0, col="blue")
