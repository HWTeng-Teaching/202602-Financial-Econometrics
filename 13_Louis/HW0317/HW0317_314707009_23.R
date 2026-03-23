#3.23
#(a.)
rm(list=ls())
library(POE5Rdata)
data("collegetown")
collegetown

model <- lm(price ~ I(sqft^2), data = collegetown)
smod <- summary(model)
smod

alpha2_hat <- coef(model)["I(sqft^2)"]
se_alpha2  <- smod$coefficients["I(sqft^2)", "Std. Error"]
df   <- df.residual(model)

alpha_a <- 0.05

sqft_a <- 20
me_a   <- 2 * alpha2_hat * sqft_a
se_me_a <- 2 * sqft_a * se_alpha2
t_a <- (me_a - 13) / se_me_a
tc_a <- qt(1 - alpha_a, df) 
p_value_a <- 1 - pt(t_a, df)



cat("虛無假設 H0: me <= 13\n")
cat("對立假設 H1: me > 13\n")
cat("t 值為", t_a, "\n")
cat("拒絕域為 t 大於", tc_a, "\n")
cat("p-value:", p_value_a,"")

if (t_a > tc_a && p_value_a < alpha_a) {
  cat("結論：拒絕虛無假說 (Reject H0)。數據顯示2000 平方英尺房子的邊際效應超過 $13,000。\n")
} else {
  cat("結論：無法拒絕虛無假說 (Fail to reject H0)。\n")
}

#(b.)
sqft_b <- 40
me_b   <- 2 * alpha2_hat * sqft_b
se_me_b <- 2 * sqft_b * se_alpha2
t_b <- (me_b - 13) / se_me_b
tc_b <- qt(1 - alpha_a, df) 
p_value_b <- 1 - pt(t_b, df)



cat("虛無假設 H0: me <= 13\n")
cat("對立假設 H1: me > 13\n")
cat("t 值為", t_b, "\n")
cat("拒絕域為 t 大於", tc_b, "\n")
cat("p-value:", p_value_b,"")

if (t_b > tc_b && p_value_b < alpha_a) {
  cat("結論：拒絕虛無假說 (Reject H0)。數據顯示4000 平方英尺房子的邊際效應超過 $13,000。\n")
} else {
  cat("結論：無法拒絕虛無假說 (Fail to reject H0)。\n")
}

#(c.)
new_data <- data.frame(sqft = 20)
ci <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
print(ci)

#(d.)
sample_20 <- subset(collegetown, sqft == 20)
print(sample_20$price)
sample_mean_20 <- mean(sample_20$price)
cat("樣本中 SQFT=20 的平均房價為：", sample_mean_20, "\n")
