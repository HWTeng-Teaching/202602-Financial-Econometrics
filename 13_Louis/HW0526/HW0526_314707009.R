rm(list=ls()) #Removes all items in Environment!
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(POE5Rdata)
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 
library(AER)
library(xtable)
library(stargazer)

data("lasvegas", package="PoEdata")

#a
# 估計 LPM 模型
lpm_model <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, data = lasvegas)

# 使用 White heteroskedasticity robust standard errors (HC1)
coeftest(lpm_model, vcov = vcovHC(lpm_model, type = "HC1"))

#b
logit_model <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, 
                   family = binomial(link = "logit"), data = lasvegas)
summary(logit_model)

#c
# LPM 預測
predict(lpm_model, newdata = lasvegas[c(500, 1000), ])

# Logit 預測 (務必加上 type="response" 才會輸出機率)
predict(logit_model, newdata = lasvegas[c(500, 1000), ], type = "response")


#d
hist(lasvegas$credit, main="Histogram of CREDIT", xlab="Credit Score")

# 建立預測情境資料框
scenario_d <- data.frame(credit = c(500, 600, 700), amount = 2.5, lvr = 80, 
                         rate = 8, term = 30, ref = 0, insur = 0, arm = 0)

# 計算 LPM 與 Logit 的機率
prob_lpm_d <- predict(lpm_model, newdata = scenario_d)
prob_logit_d <- predict(logit_model, newdata = scenario_d, type = "response")

data.frame(credit = c(500, 600, 700), LPM_Prob = prob_lpm_d, Logit_Prob = prob_logit_d)

#e
scenario_e <- data.frame(credit = c(500, 600, 700), amount = 2.5, lvr = 80, 
                         rate = 8, term = 30, ref = 0, insur = 0, arm = 0)
# LPM 的邊際效果 (即 CREDIT 變數的迴歸係數)
me_lpm <- coef(lpm_model)["credit"]

# Logit 的邊際效果計算
# 1. 先計算在該情境下的預測機率 P
prob_logit_e <- predict(logit_model, newdata = scenario_e, type = "response")
# 2. 提取 Logit 模型中 CREDIT 的係數 gamma
gamma_credit <- coef(logit_model)["credit"]
# 3. 套用公式: ME = P * (1 - P) * gamma
me_logit <- prob_logit_e * (1 - prob_logit_e) * gamma_credit

# 整合並列印結果
result_e <- data.frame(
  CREDIT = c(500, 600, 700),
  ME_LPM = rep(me_lpm, 3), # LPM 邊際效果為常數
  ME_Logit = me_logit      # Logit 邊際效果會變動
)
print(result_e)

#f
# 繪製 LVR 直方圖
hist(lasvegas$lvr, main="Histogram of Loan-to-Value Ratio (LVR)", 
     xlab="LVR (%)", col="lightblue", breaks=20)

# 設定 LVR = 20 與 80 的情境 (其餘條件如前)
scenario_f <- data.frame(lvr = c(20, 80), credit = 600, amount = 2.5, 
                         rate = 8, term = 30, ref = 0, insur = 0, arm = 0)

# 計算 LPM 與 Logit 預測機率
prob_lpm_f <- predict(lpm_model, newdata = scenario_f)
prob_logit_f <- predict(logit_model, newdata = scenario_f, type = "response")

# 整合並列印結果
result_f <- data.frame(
  LVR = c(20, 80),
  Prob_LPM = prob_lpm_f,
  Prob_Logit = prob_logit_f
)
print(result_f)

#g
# 建立預測分類結果 (預測機率 >= 0.5 則設為 1，否則為 0)
class_lpm <- ifelse(predict(lpm_model) >= 0.5, 1, 0)
class_logit <- ifelse(predict(logit_model, type="response") >= 0.5, 1, 0)

# 計算準確率 (與真實 DELINQUENT 相同的比例)
accuracy_lpm <- mean(class_lpm == lasvegas$delinquent)
accuracy_logit <- mean(class_logit == lasvegas$delinquent)

# 列印準確率
cat("LPM 準確率 (Threshold=0.5):", round(accuracy_lpm * 100, 2), "%\n")
cat("Logit 準確率 (Threshold=0.5):", round(accuracy_logit * 100, 2), "%\n")
