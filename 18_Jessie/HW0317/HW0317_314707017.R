#3.23

#(a)
library(POE5Rdata)
data(collegetown)
model = lm(price ~ I(sqft^2), data = collegetown)
summary(model)
# SQFT=20的margin effect and se
aphla2 <- coef(model)[[2]]
marginal_effect <- 2*aphla2*20
se <- 2*20*0.005256
# test
t_stat <- (margine_effect-13)/se
df <- df.residual(model)  #此函數提取自由度
#計算 p-value (右尾)
p_value <- pt(t_stat, df, lower.tail = FALSE)
# 輸出結果
print(paste("t-value =", t_stat))
print(paste("Marginal Effect =", marginal_effect))
print(paste("p-value =", p_value))


#(b)
# SQFT=40的margin effect and se
aphla2 <- coef(model)[[2]]
marginal_effect_b <- 2*aphla2*40
se <- 2*40*0.005256
# test
t_stat <- (marginal_effect_b-13)/se
df <- df.residual(model)  #此函數提取自由度
#計算 p-value (右尾)
p_value <- pt(t_stat, df, lower.tail = FALSE)
# 輸出結果
print(paste("t-value =", t_stat))
print(paste("Marginal Effect =", marginal_effect))
print(paste("p-value =", p_value))


#(c)
new_data <- data.frame(sqft = 20)
CI <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
print(CI)


#(d)
price<- collegetown$price
str(subset(collegetown,sqft == 20)$price)
summary(subset(collegetown,sqft == 20)$price)



#3.31
#(a)
install.packages("ggplot2")
library(POE5Rdata)
library(ggplot2)
data(tuna)
summary(tuna$sal1)
summary(tuna$apr1)
sd(tuna$sal1)
sd(tuna$apr1)

# 銷售量隨週數變動圖 (SAL1 vs WEEK)
tuna$week <- 1:nrow(tuna)
p1 <- ggplot(tuna, aes(x = week, y = sal1)) +
  geom_line(color = "blue") + 
  geom_point() +
  labs(title = "Weekly Sales of Brand 1 Tuna", y = "Unit Sales", x = "Week") +
  theme_minimal()
# 價格隨週數變動圖 (APR1 vs WEEK)
p2 <- ggplot(tuna, aes(x = week, y = apr1)) +
  geom_line(color = "purple") + 
  geom_point() +
  labs(title = "Weekly Price of Brand 1 Tuna", y = "Price per Can ($)", x = "Week") +
  theme_minimal()
# 顯示圖表
p1
p2


#(b)
sal1=tuna$sal1
apr1=tuna$apr1
plot(x=apr1,y=sal1,main="Price to sales",xlab="price",ylab="sales")


#(c)
# 建立新變數 PRICE1 (將美元 APR1 換算成美分)
PRICE1 <- 100 * apr1
#進行線性迴歸分析 (SAL1 = b1 + b2*PRICE1 + e)
model <- lm(sal1 ~ PRICE1, data = tuna)
summary(model)
#提取Point Estimate for b2
b2_hat <- coef(model)["PRICE1"]
CI <- confint(model, "PRICE1", level = 0.95)
print(CI)


#(d)
new_data <- data.frame(PRICE1 = 70)
CI <- predict(model, newdata = new_data, interval = "confidence", level = 0.9)
print(CI)


#(e)
avg_price <- mean(PRICE1)
avg_sales <- mean(sal1)
# 提取斜率項 b2 及Standard Error
b2_hat <- coef(model)["PRICE1"]
se_b2 <- summary(model)$coefficients["PRICE1", "Std. Error"]
# 彈性 = 斜率 * (平均價格 / 平均銷量)
elasticity_hat <- b2_hat * (avg_price / avg_sales)
# 因為平均值被視為常數，彈性的標準誤 = se(b2) * (平均價格 / 平均銷量)
se_elasticity <- se_b2 * (avg_price / avg_sales)
# 查 t 分配臨界值 (df = n - 2 = 52 - 2 = 50)
t_crit <- qt(0.975, df = df.residual(model))
lower_elasticity <- elasticity_hat - t_crit * se_elasticity
upper_elasticity <- elasticity_hat + t_crit * se_elasticity

cat("Elasticity:", elasticity_hat, "\n")
cat("95% CI: [", lower_elasticity, ",", upper_elasticity, "]\n")


#(f)
#avg_price <- mean(PRICE1)
#avg_sales <- mean(sal1)
#b2_hat <- coef(model)["PRICE1"]
#se_b2 <- summary(model)$coefficients["PRICE1", "Std. Error"]
#elasticity_hat <- b2_hat * (avg_price / avg_sales)
#se_elasticity <- se_b2 * (avg_price / avg_sales)

# 計算t統計量
t_statistic <- (elasticity_hat - (-3)) / se_elasticity
# 計算p值
df_tuna <- df.residual(model)
p_value <- 2 * (1 - pt(abs(t_statistic), df = df_tuna))
# 判斷拒絕域 (10% 顯著水準)
alpha <- 0.10
t_critical <- qt(1 - alpha/2, df = df_tuna)

print(paste("t-value =", t_statistic))
print(paste("t_critical=", t_critical))
print(paste("p-value =", p_value))

