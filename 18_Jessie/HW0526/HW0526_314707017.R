#16.18
install.packages(c("lmtest", "sandwich", "mfx", "ggplot2", "caret"))

library(lmtest)    # 用於 coeftest 穩健標準誤檢定
library(sandwich)  # 用於 vcovHC 計算 White 異質變數穩健變異數
library(mfx)       # 用於直接計算 Logit 的邊際效應 (logitmfx)
library(ggplot2)   # 用於繪製高質量直方圖
library(caret)     # 用於混淆矩陣與預測正確率計算

# a. 線性機率模型 (LPM) 估計（變數改為小寫，包含 White 穩健標準誤）
lpm_model <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, data = lasvegas)

cat("--- (a) 線性機率模型 (LPM) 估計結果 (含 White 穩健標準誤) ---\n")
lpm_robust <- coeftest(lpm_model, vcov = vcovHC(lpm_model, type = "HC1"))
print(lpm_robust)


# b. Logit 模型估計
logit_model <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, 
                   data = lasvegas, family = binomial(link = "logit"))

cat("\n--- (b) Logit 模型估計結果 ---\n")
print(summary(logit_model))



# c. 計算第 500 筆與第 1000 筆資料的預測值
obs_500_1000 <- lasvegas[c(500, 1000), ]

# LPM 預測
pred_lpm_c <- predict(lpm_model, newdata = obs_500_1000)
# Logit 預測
pred_logit_c <- predict(logit_model, newdata = obs_500_1000, type = "response")

cat("\n--- (c) 第 500 筆與第 1000 筆的預測機率 ---\n")
print(data.frame(
  Observation = c(500, 1000),
  Actual_Delinquent = obs_500_1000$delinquent,
  LPM_Predicted_P = pred_lpm_c,
  Logit_Predicted_P = pred_logit_c
))



# d. 繪製 credit 直方圖並預測特定條件下的違約機率
credit_plot <- ggplot(lasvegas, aes(x = credit)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of CREDIT (Credit Score)", x = "Credit Score", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

credit_plot

# 設定特定情境數據并預測 (credit = 500, 600, 700)
scenario_d <- data.frame(
  credit = c(500, 600, 700),
  amount = 2.5,
  lvr = 80,
  rate = 8,
  ref = 0,
  insur = 0,
  term = 30,
  arm = 0
)

pred_lpm_d <- predict(lpm_model, newdata = scenario_d)
pred_logit_d <- predict(logit_model, newdata = scenario_d, type = "response")

cat("\n--- (d) 特定條件下不同 credit 的違約機率預測 ---\n")
print(data.frame(
  credit = c(500, 600, 700),
  LPM_Prob = pred_lpm_d,
  Logit_Prob = pred_logit_d
))


# e. 計算 credit 在 500, 600, 700 分下的邊際效應
me_lpm <- coef(lpm_model)["credit"]

beta_credit <- coef(logit_model)["credit"]
me_logit_500 <- beta_credit * pred_logit_d[1] * (1 - pred_logit_d[1])
me_logit_600 <- beta_credit * pred_logit_d[2] * (1 - pred_logit_d[2])
me_logit_700 <- beta_credit * pred_logit_d[3] * (1 - pred_logit_d[3])

cat("\n--- (e) credit 在不同分數下的邊際效應 ---\n")
cat("LPM 邊際效應 (固定):", me_lpm, "\n")
cat("Logit 邊際效應 (credit=500):", me_logit_500, "\n")
cat("Logit 邊際效應 (credit=600):", me_logit_600, "\n")
cat("Logit 邊際效應 (credit=700):", me_logit_700, "\n")


# f. 繪製 lvr 直方圖並計算 lvr = 20 與 80 的違約機率
lvr_plot <- ggplot(lasvegas, aes(x = lvr)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of LVR (Loan-to-Value Ratio)", x = "LVR (%)", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

lvr_plot

# 設定特定情境數據并預測 (lvr = 20, 80)
scenario_f <- data.frame(
  lvr = c(20, 80),
  credit = 600,
  amount = 2.5,
  rate = 8,
  ref = 0,
  insur = 0,
  term = 30,
  arm = 0
)

pred_lpm_f <- predict(lpm_model, newdata = scenario_f)
pred_logit_f <- predict(logit_model, newdata = scenario_f, type = "response")

cat("\n--- (f) 不同 LVR 的違約機率預測 ---\n")
print(data.frame(
  lvr = c(20, 80),
  LPM_Prob = pred_lpm_f,
  Logit_Prob = pred_logit_f
))


# g. 比較兩模型的預測正確率 (分類門檻值 = 0.5)
# 1. 取得全樣本的預測機率
full_pred_lpm   <- predict(lpm_model)
full_pred_logit <- predict(logit_model, type = "response")

# 2. 以 0.5 為門檻值，將機率轉為 0 或 1 的預測分類
class_lpm   <- ifelse(full_pred_lpm >= 0.5, 1, 0)
class_logit <- ifelse(full_pred_logit >= 0.5, 1, 0)

# 3. 建立混淆矩陣 (列為預測值，欄為真實違約狀態)
matrix_lpm   <- table(Predicted = class_lpm, Actual = lasvegas$delinquent)
matrix_logit <- table(Predicted = class_logit, Actual = lasvegas$delinquent)

# 4. 計算整體預測正確率 (對角線數值總和 / 總樣本數)
acc_lpm   <- sum(diag(matrix_lpm)) / sum(matrix_lpm)
acc_logit <- sum(diag(matrix_logit)) / sum(matrix_logit)

cat("\n--- (g) 全樣本預測正確率比較 (門檻值 = 0.5) ---\n")
cat("LPM 混淆矩陣：\n")
print(matrix_lpm)
cat("LPM 整體正確率 (Accuracy)：", round(acc_lpm, 4), " (即 ", round(acc_lpm*100, 2), "%)\n\n", sep="")

cat("Logit 混淆矩陣：\n")
print(matrix_logit)
cat("Logit 整體正確率 (Accuracy)：", round(acc_logit, 4), " (即 ", round(acc_logit*100, 2), "%)\n", sep="")


# h. 授信主管決策：將 Logit 門檻值大幅調降至 0.15 的效果評估
# 1. 以 0.15 作為更嚴格的防禦型放貸門檻 (只要違約機率 >= 15% 就預測為 1 拒絕放貸)
class_logit_strict <- ifelse(full_pred_logit >= 0.15, 1, 0)

# 2. 建立新的混淆矩陣
matrix_strict <- table(Predicted = class_logit_strict, Actual = lasvegas$delinquent)

# 3. 計算調降門檻後的指標
acc_strict <- sum(diag(matrix_strict)) / sum(matrix_strict)

# 計算捕捉率/敏感度 (實際有違約的人當中，被模型成功抓出來的比例)
# 公式：矩陣中 (Predicted=1, Actual=1) / 實際違約總數 (Actual=1 的加總)
catch_rate_05 <- matrix_logit["1", "1"] / sum(lasvegas$delinquent == 1)
catch_rate_15 <- matrix_strict["1", "1"] / sum(lasvegas$delinquent == 1)

cat("\n--- (h) 調整 Logit 分類門檻值至 0.15 的實務評估 ---\n")
cat("更嚴格門檻 (0.15) 的 Logit 混淆矩陣：\n")
print(matrix_strict)
cat("門檻 0.15 下的整體正確率：", round(acc_strict, 4), "\n", sep="")
cat("【實務決策核心對比】\n")
cat("  - 當門檻設為 0.50 時，模型成功抓出高風險違約戶的比例為：", round(catch_rate_05 * 100, 2), "%\n", sep="")
cat("  - 當門檻調降至 0.15 時，模型成功抓出高風險違約戶的比例為：", round(catch_rate_15 * 100, 2), "%\n", sep="")
cat(">>> 結論：雖然整體正確率可能微幅下降，但成功攔截潛在呆帳的機率大幅???升，更符合授信風控利益！\n")