rm(list=ls()) #Removes all items in Environment!
library(lmtest) #for coeftest() and bptest().
library(broom) #for glance() and tidy()
library(POE5Rdata) #for PoE4 datasets
library(car) #for hccm() robust standard errors
library(sandwich)
library(knitr)
library(stargazer)

#a.

data("vacation",package="POE5Rdata")
mod1 <- lm(miles~income+age+kids, data=vacation)
smo1 <- summary(mod1)

confint(mod1, parm = "kids", level = 0.95)

#b.

par(mfrow = c(1, 2))
# 取出殘差
residuals_ols <- resid(mod1)

# 畫圖：殘差 vs INCOME
plot(vacation$income, residuals_ols, 
     main = "Residuals vs INCOME", 
     xlab = "INCOME", ylab = "Residuals")
abline(h = 0, col = "red") # 加入零基線

# 畫圖：殘差 vs AGE
plot(vacation$age, residuals_ols, 
     main = "Residuals vs AGE", 
     xlab = "AGE", ylab = "Residuals")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

#c.
# 1. 依照 INCOME 將資料從小到大排序
data_sorted <-vacation[order(vacation$income), ]

# 2. 切割資料集
data_low <- data_sorted[1:90, ]       # 前 90 筆
data_high <- data_sorted[111:200, ]   # 後 90 筆

# 3. 分別跑 OLS
model_low <- lm(miles ~ income + age + kids, data = data_low)
model_high <- lm(miles ~ income + age + kids, data = data_high)

# 4. 手動計算 F 統計量 (高變異的 RSS / 低變異的 RSS)
rss_low <- sum(resid(model_low)^2)
rss_high <- sum(resid(model_high)^2)
df <- 90 - 4 # n - k

# 計算 F 值
F_stat <- (rss_high / df) / (rss_low / df)

alpha <- 0.05
#找 critical F
Flc <- qf(alpha/2, df, df)
Fuc <- qf(1-alpha/2, df, df)
# 找 p-value
p_value <- 1 - pf(F_stat, df, df)
print(paste("F-statistic:", F_stat, "p-value:", p_value))

#d.
# 1. 計算穩健標準誤共變異數矩陣 (通常使用 HC1 型態)
cov_robust <- hccm(mod1, type = "hc1")

# 2. 顯示穩健標準誤下的係數檢定
robust_results <- coeftest(mod1, vcov. = cov_robust)
print(robust_results)

# 3. 手動計算 KIDS 係數的 95% 穩健信賴區間
beta_kids <- coef(mod1)["kids"]
se_kids_robust <- sqrt(cov_robust["kids", "kids"])
t_cr <- qt(0.975, df = 196)

ci_lower_robust <- beta_kids - t_cr * se_kids_robust
ci_upper_robust <- beta_kids + t_cr * se_kids_robust
print(c(ci_lower_robust, ci_upper_robust))

#e.
# 1. 定義權重 w = 1 / (INCOME^2)
weights_gls <- 1 / (vacation$income^2)

# 2. 執行 GLS (加權最小平方法 WLS)
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights_gls)

# 3. 計算傳統 GLS 的 95% 信賴區間
confint(model_gls, parm = "kids", level = 0.95) # 這是第一個答案

# 4. 計算穩健 GLS 的 95% 信賴區間
cov_gls_robust <- hccm(model_gls, type = "hc1")

beta_kids_gls <- coef(model_gls)["kids"]
se_kids_gls_robust <- sqrt(cov_gls_robust["kids", "kids"])

ci_lower_gls_robust <- beta_kids_gls - t_cr * se_kids_gls_robust
ci_upper_gls_robust <- beta_kids_gls + t_cr * se_kids_gls_robust
print(c(ci_lower_gls_robust, ci_upper_gls_robust)) # 這是第二個答案

