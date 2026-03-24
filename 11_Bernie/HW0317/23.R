library(POE5Rdata)
data("collegetown")

# 模型
mod <- lm(price ~ I(sqft^2), data = collegetown)
summary(mod)

# 取出 a2 與標準誤（轉成純數字）
b2 <- as.numeric(coef(mod)["I(sqft^2)"])
se_b2 <- as.numeric(coef(summary(mod))["I(sqft^2)", "Std. Error"])
df <- df.residual(mod)

b1 <- coef(mod)[1]
print(b1)

# 單尾 5% 臨界值
t_crit <- qt(0.95, df)

# 檢定函數
# H0: marginal effect <= 13
# H1: marginal effect > 13
test_ME <- function(x0, threshold = 13) {
  me_hat <- 2 * b2 * x0
  se_me  <- 2 * x0 * se_b2
  t_stat <- (me_hat - threshold) / se_me
  p_val  <- 1 - pt(t_stat, df)
  reject <- t_stat > t_crit
  
  c(
    sqft_value = x0,
    marginal_effect_hat = me_hat,
    std_error = se_me,
    t_stat = t_stat,
    critical_value = t_crit,
    p_value = p_val,
    reject_H0 = reject
  )
}

# (a) 2000 sqft = 20
res_a <- test_ME(20)

# (b) 4000 sqft = 40
res_b <- test_ME(40)

# 輸出
cat("Part (a): 2000 sqft house\n")
cat("Estimated marginal effect =", round(res_a["marginal_effect_hat"], 4), "\n")
cat("t statistic =", round(res_a["t_stat"], 4), "\n")
cat("critical value =", round(res_a["critical_value"], 4), "\n")
cat("p-value =", round(res_a["p_value"], 6), "\n")
cat("Reject H0? ", as.logical(res_a["reject_H0"]), "\n\n")

cat("Part (b): 4000 sqft house\n")
cat("Estimated marginal effect =", round(res_b["marginal_effect_hat"], 4), "\n")
cat("t statistic =", round(res_b["t_stat"], 4), "\n")
cat("critical value =", round(res_b["critical_value"], 4), "\n")
cat("p-value =", round(res_b["p_value"], 6), "\n")
cat("Reject H0? ", as.logical(res_b["reject_H0"]), "\n")

# (c)
predict(mod, newdata = data.frame(sqft = 20),
        interval = "confidence", level = 0.95)

# (d)
mean(collegetown$price[collegetown$sqft == 20], na.rm = TRUE)
collegetown[collegetown$sqft == 20, ]