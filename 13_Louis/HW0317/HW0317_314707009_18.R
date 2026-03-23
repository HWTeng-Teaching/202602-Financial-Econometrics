#3.18
#(a.)
rm(list=ls())
b1 <- 6.855
b2 <- 3.880
seb1 <- 7.383
seb2 <- 0.112
N <- 20

Mean_INCOME <- 59.3
Mean_INSURANCE_hat <- b1 + b2*Mean_INCOME

cat(" 根據估計式：INSURANCE_hat =", b1, "+", b2, "* INCOME\n",
    "當 INCOME 的平均值為：", Mean_INCOME, "\n",
    "計算得出 INSURANCE 的平均值為：", Mean_INSURANCE_hat, "\n")

plot(0:200, b1 + b2 * (0:200), type="l", col="blue", 
     xlab="INCOME (thousands of dollars)", ylab="INSURANCE (thousands of dollars)", main="Fitted Line and Mean Point")
points(Mean_INCOME, Mean_INSURANCE_hat, col="red", pch=19) 
text(Mean_INCOME, Mean_INSURANCE_hat, 
     labels = paste0("(", Mean_INCOME, ", ", round(Mean_INSURANCE_hat, 2), ")"), 
     pos = 4,
     col = "red", 
     font = 2,
     cex = 0.8)

#(b.)
alpha <- 0.05
df <- N - 2
tc <- qt(1 - alpha/2, df)
lowb2 <- b2 - tc * seb2
upb2 <- b2 + tc * seb2
lowb2
upb2

cat(" 點估計值為：", b2, "\n",
    "95% 信賴區間為：[", lowb2, ",", upb2, "]\n")

#(c.)
X_c <- 100  
cov_b1_b2 <- -0.746
alpha_c <- 0.01  

Y_hat_100 <- b1 + b2 * X_c

var_Y_hat <- (seb1^2) + (X_c^2 * seb2^2) + (2 * X_c * cov_b1_b2)
se_Y_hat <- sqrt(var_Y_hat)


t_c_99 <- qt(1 - alpha_c/2, df)

lowY_hat_100 <- Y_hat_100 - t_c_99 * se_Y_hat
upY_hat_100 <- Y_hat_100 + t_c_99 * se_Y_hat

cat("99% 信賴區間為：[", lowY_hat_100, ",", upY_hat_100, "]\n")

#(d.)
b2_null <- 5
alpha_d <- 0.05
t_d <- (b2 - b2_null) / seb2

tc_d <- qt(1 - alpha_d/2, df)

cat("虛無假設 H0: beta2 = 5\n")
cat("對立假設 H1: beta2 ≠ 5\n")
cat("拒絕域為絕對值t大於", tc_d, "\n")
cat("t值為", t_d, "\n")

if (abs(t_d) > tc_d) {
  cat("結論：拒絕虛無假說 (Reject H0)。數據「不支持」管理層的說法。\n")
} else {
  cat("結論：無法拒絕虛無假說 (Fail to reject H0)。數據支持管理層的說法。\n")
}

#(e.)
b2_null_e <- 1
alpha_e <- 0.01
t_e <- (b2 - b2_null_e) / seb2

tc_e <- qt(1 - alpha_e, df) 

cat("虛無假設 H0: beta2 = 1\n")
cat("對立假設 H1: beta2 > 1\n")
cat("拒絕域為 t 大於", tc_e, "\n")
cat("t 值為", t_e, "\n")

if (t_e > tc_e) {
  cat("結論：拒絕虛無假說 (Reject H0)。數據顯示保險持有量的增加幅度「顯著大於」收入的增加幅度。\n")
} else {
  cat("結論：無法拒絕虛無假說 (Fail to reject H0)。\n")
}
