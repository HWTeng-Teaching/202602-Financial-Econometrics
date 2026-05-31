# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 16 Exercise 16.18
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)
library(sandwich)
library(lmtest)
library(ggplot2)

data("vegas5")
lasvegas <- vegas5

# 欄位對照：
# DELINQUENT = default, LVR = ltv, REF = refinance,
# INSUR = underwater, RATE = rate, AMOUNT = amount,
# CREDIT = fico, TERM = term30, ARM = arm

head(lasvegas)
summary(lasvegas)

# ============================================================
# (a) 線性機率模型（LPM）with White 穩健標準誤
# ============================================================

lpm <- lm(default ~ ltv + refinance + underwater + rate + amount + fico + term30 + arm,
          data = lasvegas)

cat("\n========== (a) Linear Probability Model ==========\n")
cat("--- OLS coefficients with White robust SE ---\n")
lpm_robust <- coeftest(lpm, vcov = vcovHC(lpm, type = "HC1"))
print(lpm_robust)

# ============================================================
# (b) Logit 模型
# ============================================================

logit <- glm(default ~ ltv + refinance + underwater + rate + amount + fico + term30 + arm,
             data = lasvegas,
             family = binomial(link = "logit"))

cat("\n========== (b) Logit Model ==========\n")
print(summary(logit))

# ============================================================
# (c) 預測第 500 與第 1000 筆觀測值
# ============================================================

cat("\n========== (c) Predicted values for obs 500 and 1000 ==========\n")

obs_500  <- lasvegas[500, ]
obs_1000 <- lasvegas[1000, ]

lpm_pred_500    <- predict(lpm,   newdata = obs_500)
lpm_pred_1000   <- predict(lpm,   newdata = obs_1000)
logit_pred_500  <- predict(logit, newdata = obs_500,  type = "response")
logit_pred_1000 <- predict(logit, newdata = obs_1000, type = "response")

cat("Obs 500  - Actual default:", lasvegas$default[500], "\n")
cat("  LPM predicted:  ", round(lpm_pred_500,   4), "\n")
cat("  Logit predicted:", round(logit_pred_500,  4), "\n")

cat("Obs 1000 - Actual default:", lasvegas$default[1000], "\n")
cat("  LPM predicted:  ", round(lpm_pred_1000,  4), "\n")
cat("  Logit predicted:", round(logit_pred_1000, 4), "\n")

# ============================================================
# (d) CREDIT(fico) 直方圖 + 預測機率（fico = 500, 600, 700）
# ============================================================

cat("\n========== (d) CREDIT histogram + predicted probabilities ==========\n")

p_credit <- ggplot(lasvegas, aes(x = fico)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "white") +
  labs(title = "Histogram of CREDIT (fico)", x = "Credit Score (fico)", y = "Count")
ggsave("d_credit_histogram.png", plot = p_credit, dpi = 150, width = 6, height = 4)
cat("Saved: d_credit_histogram.png\n")

# 指定條件：ltv=80, rate=8, 指標=0, term30=1(30年), amount=2.5
base_d <- data.frame(ltv = 80, refinance = 0, underwater = 0, rate = 8,
                     amount = 2.5, term30 = 1, arm = 0,
                     fico = c(500, 600, 700))

lpm_d   <- predict(lpm,   newdata = base_d)
logit_d <- predict(logit, newdata = base_d, type = "response")

cat("\nfico | LPM prob | Logit prob\n")
for (i in 1:3) {
  cat(sprintf(" %d  |  %.4f  |   %.4f\n",
              base_d$fico[i], lpm_d[i], logit_d[i]))
}

# ============================================================
# (e) CREDIT(fico) 的邊際效果（fico = 500, 600, 700）
# ============================================================

cat("\n========== (e) Marginal effect of CREDIT ==========\n")

lpm_me_credit <- coef(lpm)["fico"]
cat("LPM marginal effect of fico (constant):", round(lpm_me_credit, 6), "\n")

logit_coef_credit <- coef(logit)["fico"]
logit_me_credit   <- logit_d * (1 - logit_d) * logit_coef_credit

cat("\nfico | Logit ME\n")
for (i in 1:3) {
  cat(sprintf(" %d  |  %.6f\n", base_d$fico[i], logit_me_credit[i]))
}

# ============================================================
# (f) LVR(ltv) 直方圖 + 預測機率（ltv = 20, 80）
# ============================================================

cat("\n========== (f) LVR histogram + predicted probabilities ==========\n")

p_lvr <- ggplot(lasvegas, aes(x = ltv)) +
  geom_histogram(binwidth = 5, fill = "coral", color = "white") +
  labs(title = "Histogram of LVR (ltv)", x = "Loan-to-Value Ratio (%)", y = "Count")
ggsave("f_lvr_histogram.png", plot = p_lvr, dpi = 150, width = 6, height = 4)
cat("Saved: f_lvr_histogram.png\n")

base_f <- data.frame(ltv = c(20, 80), refinance = 0, underwater = 0, rate = 8,
                     amount = 2.5, term30 = 1, arm = 0, fico = 600)

lpm_f   <- predict(lpm,   newdata = base_f)
logit_f <- predict(logit, newdata = base_f, type = "response")

cat("\nltv | LPM prob | Logit prob\n")
for (i in 1:2) {
  cat(sprintf(" %d  |  %.4f  |   %.4f\n",
              base_f$ltv[i], lpm_f[i], logit_f[i]))
}

# ============================================================
# (g) 正確預測百分比（門檻 0.5）
# ============================================================

cat("\n========== (g) Prediction accuracy (threshold = 0.5) ==========\n")

lpm_all_pred   <- predict(lpm)
logit_all_pred <- predict(logit, type = "response")

lpm_class   <- ifelse(lpm_all_pred   >= 0.5, 1, 0)
logit_class <- ifelse(logit_all_pred >= 0.5, 1, 0)

actual    <- lasvegas$default
lpm_acc   <- mean(lpm_class   == actual)
logit_acc <- mean(logit_class == actual)

cat("LPM   correct prediction rate:", round(lpm_acc,   4), "\n")
cat("Logit correct prediction rate:", round(logit_acc, 4), "\n")

cat("\nLPM confusion matrix:\n")
print(table(Predicted = lpm_class, Actual = actual))
cat("\nLogit confusion matrix:\n")
print(table(Predicted = logit_class, Actual = actual))

# ============================================================
# (h) 訓練前 500 筆，驗證後 500 筆，尋找最佳門檻
# ============================================================

cat("\n========== (h) Train on obs 1-500, evaluate on obs 501-1000 ==========\n")

train <- lasvegas[1:500,   ]
test  <- lasvegas[501:1000, ]

logit_train <- glm(default ~ ltv + refinance + underwater + rate + amount + fico + term30 + arm,
                   data  = train,
                   family = binomial(link = "logit"))

test_pred_prob <- predict(logit_train, newdata = test, type = "response")
actual_test    <- test$default

# 比較不同門檻下的正確率
thresholds <- seq(0.1, 0.9, by = 0.05)
results <- data.frame(threshold = thresholds,
                      accuracy  = NA,
                      precision = NA,
                      recall    = NA)

for (i in seq_along(thresholds)) {
  thresh <- thresholds[i]
  pred   <- ifelse(test_pred_prob >= thresh, 1, 0)
  # 以貸款人角度：預測不違約(pred=0)才核准，實際不違約(actual=0)為正確決策
  tp  <- sum(pred == 0 & actual_test == 0)  # 正確核准
  fp  <- sum(pred == 0 & actual_test == 1)  # 錯誤核准（實際違約）
  tn  <- sum(pred == 1 & actual_test == 1)  # 正確拒絕
  fn  <- sum(pred == 1 & actual_test == 0)  # 錯誤拒絕
  acc  <- mean(pred == actual_test)
  prec <- ifelse((tp + fp) > 0, tp / (tp + fp), NA)
  rec  <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
  results$accuracy[i]  <- round(acc,  4)
  results$precision[i] <- round(prec, 4)
  results$recall[i]    <- round(rec,  4)
}

cat("\nThreshold comparison on test set (obs 501-1000):\n")
print(results)

pred_05 <- ifelse(test_pred_prob >= 0.5, 1, 0)
cat("\nWith threshold 0.5:\n")
cat("Accuracy:", round(mean(pred_05 == actual_test), 4), "\n")
cat("Confusion matrix:\n")
print(table(Predicted = pred_05, Actual = actual_test))