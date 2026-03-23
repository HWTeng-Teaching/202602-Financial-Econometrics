library(dplyr)

# 1. 讀取資料
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))

# 2. 建立包含 price1 的資料表
data_tuna_b <- tuna %>% 
  mutate(week = 1:52, 
         price1 = 100 * apr1)

# 3. 建立迴歸模型 (你代碼裡的 lm_331)
lm_331 <- lm(sal1 ~ price1, data = data_tuna_b)

# 4. 提取需要的係數與平均值
# 從模型中抓出 price1 的係數 (也就是公式裡的 beta 2)
b2_331 <- coef(lm_331)["price1"] 

# 計算價格與銷售量的平均值 (記得要加上 data_tuna_b$)
mean_price <- mean(data_tuna_b$price1)
mean_sal <- mean(data_tuna_b$sal1)

# 5. 計算彈性 (Elasticity)
ela <- b2_331 * (mean_price / mean_sal)

# 6. 計算彈性的標準誤 (Standard Error) 與信賴區間
se_b2 <- summary(lm_331)$coefficients['price1', "Std. Error"]
se_ela <- se_b2 * (mean_price / mean_sal)

alpha <- 0.05
df <- lm_331$df.residual
t_value <- qt(1 - alpha/2, df)

ci_lower <- ela - t_value * se_ela
ci_upper <- ela + t_value * se_ela

# 7. 印出最終結果
cat("Elasticity:", ela, "\n")
cat("95% CI: [", ci_lower, ",", ci_upper, "]\n")