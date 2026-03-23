library(dplyr)

# 1. 讀取資料
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

# 2. 建立迴歸模型
model <- lm(price ~ I(sqft^2), data = collegetown)

# 3. 設定預測條件與顯著水準
sqft <- 20
alpha <- 0.05
df <- model$df.residual           
tc <- qt(1 - alpha/2, df)

# 4. 從模型提取係數 (alpha1, alpha2) 與變異數-共變異數矩陣的元素
alpha1 <- model$coef[1]
alpha2 <- model$coef[2]

vara1 <- vcov(model)[1, 1]
vara2 <- vcov(model)[2, 2]
cova1a2 <- vcov(model)[1, 2]

# 5. 計算期望價格與變異數
exp_price <- alpha1 + alpha2 * (sqft^2)
varL <- vara1 + ((sqft^2)^2) * vara2 + 2 * (sqft^2) * cova1a2 
seL <- sqrt(varL)

# 6. 計算預測區間 (Interval estimate)
lower_bound <- exp_price - tc * seL
upper_bound <- exp_price + tc * seL
interval <- c(lower_bound, upper_bound)

# 印出最終的區間範圍
interval