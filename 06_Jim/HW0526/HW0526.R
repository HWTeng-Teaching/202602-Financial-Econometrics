rm(list=ls())
# library(POE5Rdata)
library(stargazer)
# stargazer(results, summary=FALSE, type="latex", 
#           title="Simulation Results", 
#           header=FALSE)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(sandwich)
library(PoEdata)
data(lasvegas)
data <- lasvegas

# 3. 定義模型公式
fml <- delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm

# ==========================================
# a. 線性機率模型 (LPM) 估計（含 White 穩健標準誤）
# ==========================================
lpm_model <- lm(fml, data = data)
cat("--- a. LPM Model Estimation (Robust SE) ---\n")
print(coeftest(lpm_model, vcov = vcovHC(lpm_model, type = "HC1")))
stargazer(coeftest(lpm_model, vcov = vcovHC(lpm_model, type = "HC1")), summary=FALSE, type="latex",
          title="(a) lpm_model",
          header=FALSE)

# ==========================================
# b. Logit 模型估計
# ==========================================
logit_model <- glm(fml, data = data, family = binomial(link = "logit"))
cat("\n--- b. Logit Model Estimation ---\n")
print(summary(logit_model))
stargazer(logit_model, summary=FALSE, type="latex",
          title="(b) logit_model",
          header=FALSE)


# ==========================================
# c. 計算第 500 與 1000 筆觀測值的預測值
# ==========================================
obs_500_1000 <- data[c(500, 1000), ]
pred_c_lpm   <- predict(lpm_model, newdata = obs_500_1000)
pred_c_logit <- predict(logit_model, newdata = obs_500_1000, type = "response")

cat("\n--- c. Predictions for Obs 500 and 1000 ---\n")
print(data.frame(LPM = pred_c_lpm, Logit = pred_c_logit, row.names = c("Obs 500", "Obs 1000")))

# ==========================================
# d. CREDIT 變化下的機率預測
# ==========================================
# 畫直方圖
hist(data$credit, main="Histogram of CREDIT", xlab="Credit Score", col="lightblue", border="white")

# 建立場景資料
scenario_d <- data.frame(lvr=80, ref=0, insur=0, rate=8, amount=2.5, 
                         credit=c(500, 600, 700), term=30, arm=0)

prob_d_lpm   <- predict(lpm_model, newdata = scenario_d)
prob_d_logit <- predict(logit_model, newdata = scenario_d, type = "response")

cat("\n--- d. Predicted Probabilities for CREDIT = 500, 600, 700 ---\n")
print(data.frame(CREDIT = c(500, 600, 700), LPM_Prob = prob_d_lpm, Logit_Prob = prob_d_logit))

# ==========================================
# e. 計算 CREDIT 的邊際效應 (Marginal Effect)
# ==========================================
me_e_lpm   <- coef(lpm_model)["credit"]
me_e_logit <- prob_d_logit * (1 - prob_d_logit) * coef(logit_model)["credit"]

cat("\n--- e. Marginal Effects of CREDIT ---\n")
print(data.frame(CREDIT = c(500, 600, 700), LPM_ME = me_e_lpm, Logit_ME = me_e_logit))

# ==========================================
# f. LVR 變化下的機率預測
# ==========================================
# 畫直方圖
hist(data$lvr, main="Histogram of LVR", xlab="Loan-to-Value Ratio", col="lightgreen", border="white")

scenario_f <- data.frame(lvr=c(20, 80), ref=0, insur=0, rate=8, amount=2.5, 
                         credit=600, term=30, arm=0)

prob_f_lpm   <- predict(lpm_model, newdata = scenario_f)
prob_f_logit <- predict(logit_model, newdata = scenario_f, type = "response")

cat("\n--- f. Predicted Probabilities for LVR = 20, 80 ---\n")
print(data.frame(LVR = c(20, 80), LPM_Prob = prob_f_lpm, Logit_Prob = prob_f_logit))

# ==========================================
# g. 正確預測率比較 (Threshold = 0.5)
# ==========================================
class_lpm   <- ifelse(predict(lpm_model) >= 0.5, 1, 0)
class_logit <- ifelse(predict(logit_model, type = "response") >= 0.5, 1, 0)

acc_lpm   <- mean(class_lpm == data$delinquent)
acc_logit <- mean(class_logit == data$delinquent)

cat("\n--- g. Percentage of Correct Predictions ---\n")
cat("LPM Accuracy   :", acc_lpm * 100, "%\n")
cat("Logit Accuracy :", acc_logit * 100, "%\n")
