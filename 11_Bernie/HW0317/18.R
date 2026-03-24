# 已知資訊
n <- 20
b0 <- 6.855
b1 <- 3.880
se_b0 <- 7.383
se_b1 <- 0.112
xbar <- 59.3

# (a) 樣本平均保險額
ybar <- b0 + b1 * xbar
ybar

cat("Sample mean of INCOME =", xbar, "\n")
cat("Sample mean of INSURANCE =", ybar, "\n")

# -------------------------------
# 畫 fitted line 的 sketch
# -------------------------------
x <- seq(0, 100, length.out = 200)
yhat <- b0 + b1 * x

plot(x, yhat, type = "l", lwd = 2,
     xlab = "INCOME (thousand dollars)",
     ylab = "INSURANCE (thousand dollars)",
     main = "Fitted Relationship: INSURANCE on INCOME")

# 標出截距
points(0, b0, pch = 19, col = "red")
text(0, b0, labels = paste0("Intercept = ", b0), pos = 4, col = "red")

# 標出平均點
points(xbar, ybar, pch = 19, col = "blue")
text(xbar, ybar,
     labels = paste0("Means (", xbar, ", ", round(ybar, 3), ")"),
     pos = 4, col = "blue")

# 在圖上加註 slope
legend("topleft",
       legend = c(paste("Slope =", b1),
                  paste("Intercept =", b0)),
       bty = "n")

# -------------------------------
# (b) 每增加 $1000 收入，平均保險額增加多少？
# point estimate = slope
# 95% CI for slope
# -------------------------------
df <- n - 2
t_crit <- qt(0.975, df = df)

slope_est <- b1
ci_lower <- b1 - t_crit * se_b1
ci_upper <- b1 + t_crit * se_b1

cat("\nPoint estimate for change in INSURANCE per additional $1000 INCOME =",
    slope_est, "\n")

cat("95% CI for slope = (", round(ci_lower, 4), ",", round(ci_upper, 4), ")\n")

# 解釋文字
cat("\nInterpretation:\n")
cat("For each additional $1000 of household income, the estimated average amount\n")
cat("of life insurance increases by", slope_est, "thousand dollars.\n")
cat("A 95% confidence interval is from", round(ci_lower, 4), "to",
    round(ci_upper, 4), "thousand dollars.\n")