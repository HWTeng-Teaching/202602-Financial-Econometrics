library(dplyr)

# 1. 讀取資料與準備變數
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))
data_tuna_b <- tuna %>% 
  mutate(week = 1:52, price1 = 100 * apr1)

# 2. 建立迴歸模型
reg_model <- lm(sal1 ~ price1, data = data_tuna_b)

# 3. 算出 elasticity (彈性) 和 se_elasticity (標準誤)
b2 <- coef(reg_model)["price1"] 
mean_price <- mean(data_tuna_b$price1)
mean_sal <- mean(data_tuna_b$sal1)

elasticity <- b2 * (mean_price / mean_sal)
se_b2 <- summary(reg_model)$coefficients['price1', "Std. Error"]
se_elasticity <- se_b2 * (mean_price / mean_sal)

# 4. 進行假說檢定：H0: elasticity = -3
# 計算t統計量
t_statistic <- (elasticity - (-3)) / se_elasticity

# 計算p值 (加上 -abs 保證雙尾機率計算正確)
p_value <- 2 * pt(-abs(t_statistic), df = nrow(data_tuna_b) - 2)

# 計算10%水準下的臨界值 (雙尾檢定，單邊為5%，所以用0.95)
t_critical <- qt(0.95, df = nrow(data_tuna_b) - 2)

# 5. 打印結果
print(paste("t-value =", round(t_statistic, 4)))
print(paste("t_critical=", round(t_critical, 4)))
print(paste("p-value =", format(p_value, scientific = FALSE)))