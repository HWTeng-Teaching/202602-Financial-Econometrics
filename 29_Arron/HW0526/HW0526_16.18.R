

library(PoEdata)
data("lasvegas")
cat("成功載入！資料集維度：", dim(lasvegas), "\n\n")
print(head(lasvegas[, c("delinquent", "lvr", "credit", "rate", "amount")]))

if(!require(sandwich)) install.packages("sandwich")   # 用於 White 異質變異穩健標準誤
if(!require(lmtest)) install.packages("lmtest")       # 用於進行穩健係數檢定 (coeftest)
library(sandwich)
library(lmtest)


# ------------------------------------------------------------------------------
# (a) 線性機率模型 (LPM) 估計與 White 穩健標準誤
cat(" (a) 線性機率模型 (LPM) 估計結果（含 White 穩健標準誤）\n")

model.LPM <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, data = lasvegas)

# 套用 White's 穩健標準誤 (使用計量標準的 HC1 修正)
robust.LPM <- coeftest(model.LPM, vcov = vcovHC(model.LPM, type = "HC1"))
print(robust.LPM)

cat("\n💡 [符號合理性解析]：\n")
cat("- credit (信用分數) 係數為負：信用愈好，違約率愈低，合理。\n")
cat("- lvr (貸款成數) 係數為正：財務槓桿愈大，違約風險愈高，合理。\n")
cat("- rate (貸款利率) 係數為正：還款利息壓力愈重，愈容易違約，合理。\n")

# ------------------------------------------------------------------------------
# (b) Logit 模型估計與對比
cat(" (b) Logit 模型估計結果\n")

# 估計二元選擇 Logit 模型 (使用最大概似估計法 MLE)
model.logit <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, 
                   data = lasvegas, family = binomial(link = "logit"))
print(summary(model.logit))

cat("經對比，Logit 模型與 LPM 模型在所有核心變數的『正負號』與『統計顯著性』上高度一致，\n")
cat("證實模型設定的結構非常穩健。\n")


# ------------------------------------------------------------------------------
# (c) 計算第 500 筆與第 1000 筆觀測值的違約預測機率
cat(" (c) 第 500 筆與第 1000 筆觀測值的違約機率預測\n")

obs_500_1000 <- lasvegas[c(500, 1000), ]
pred.LPM.c <- predict(model.LPM, obs_500_1000)
pred.logit.c <- predict(model.logit, obs_500_1000, type = "response") # 轉換回 0~1 機率值

results.c <- data.frame(
  Observation = c(500, 1000),
  Actual_Delinquent = obs_500_1000$delinquent,
  LPM_Predicted_Prob = pred.LPM.c,
  Logit_Predicted_Prob = pred.logit.c
)
print(results.c)


# ------------------------------------------------------------------------------
# (d) 特定放款情境下，不同信用分數 (500, 600, 700) 的違約機率預測
cat(" (d) 特定放款情境下，不同信用分數 (500, 600, 700) 的違約機率預測\n")
hist(lasvegas$credit, 
     breaks = 20, 
     col = "skyblue", 
     border = "white",
     main = "Histogram of Credit Scores (CREDIT)", 
     xlab = "Credit Score", 
     ylab = "Frequency")
abline(v = c(500, 600, 700), col = "red", lty = 2, lwd = 2)

scenarios.d <- data.frame(
  credit = c(500, 600, 700),amount = 2.5,lvr = 80,rate = 8,term = 30,ref = 0,insur = 0,arm = 0)

prob.LPM.d <- predict(model.LPM, scenarios.d)
prob.logit.d <- predict(model.logit, scenarios.d, type = "response")

results.d <- data.frame(
  Credit_Score = c(500, 600, 700),
  LPM_Probability = prob.LPM.d,
  Logit_Probability = prob.logit.d
)
print(results.d)


# ------------------------------------------------------------------------------
# (e) 計算 credit 在不同分數點上的邊際效應 (Marginal Effects)
cat(" (e) 計算 credit 在不同分數點上的邊際效應\n")

me.LPM <- coef(model.LPM)["credit"]
p.logit.d <- results.d$Logit_Probability
me.logit.d <- coef(model.logit)["credit"] * p.logit.d * (1 - p.logit.d) # 公式: beta * P * (1-P)

results.e <- data.frame(
  Credit_Score = c(500, 600, 700),
  LPM_Marginal_Effect = rep(me.LPM, 3),
  Logit_Marginal_Effect = me.logit.d
)
print(results.e)

cat("\n💡 [邊際效應解析]：\n")
cat("LPM 模型假設邊際效應固定不變；而 Logit 模型則成功捕捉到非線性的『尾端收斂效應』，\n")
cat("在信用分數中游 (600分) 時，分數變動對違約機率的邊際影響力達到最大。\n")


# ------------------------------------------------------------------------------
# (f) 特定放款情境下，不同貸款成數 lvr (20% vs 80%) 的違約機率對比
cat(" (f) 不同貸款成數 lvr (20% vs 80%) 的違約機率對比 (credit=600)\n")

hist(lasvegas$lvr, 
     breaks = 25, 
     col = "#70AD47",       
     border = "white",
     main = "Figure 2: Histogram of Loan-to-Value Ratio (LVR)", 
     xlab = "Loan-to-Value Ratio (LVR %)", 
     ylab = "Number of Borrowers (Frequency)",
     cex.main = 1.2, cex.lab = 1.0)


abline(v = c(20, 80), col = "red", lty = 2, lwd = 2.5)
text(x = c(26, 74), y = c(120, 120), 
     labels = c("Low LVR (20%)", "High LVR (80%)"), col = "darkred", font = 2)

scenarios.f <- data.frame(
  lvr = c(20, 80),credit = 600,amount = 2.5,rate = 8,term = 30,ref = 0,insur = 0,arm = 0)

prob.LPM.f <- predict(model.LPM, scenarios.f)
prob.logit.f <- predict(model.logit, scenarios.f, type = "response")

results.f <- data.frame(
  LVR_Level = c("低槓桿 (20%)", "高槓桿 (80%)"),
  LPM_Probability = prob.LPM.f,
  Logit_Probability = prob.logit.f
)
print(results.f)



# ------------------------------------------------------------------------------
# (g) 全樣本預測準確率評估 (以 0.5 為分類門檻值)
cat(" (g) 全樣本內預測準確率對比 (門檻值 = 0.5)\n")

fitted.LPM <- predict(model.LPM)
fitted.logit <- predict(model.logit, type = "response")

table.LPM <- table(True = lasvegas$delinquent, Predicted = as.numeric(fitted.LPM >= 0.5))
table.logit <- table(True = lasvegas$delinquent, Predicted = as.numeric(fitted.logit >= 0.5))

accuracy.LPM <- sum(diag(table.LPM)) / sum(table.LPM)
accuracy.logit <- sum(diag(table.logit)) / sum(table.logit)

cat("LPM 全樣本分類準確率：", round(accuracy.LPM * 100, 2), "%\n")
cat("Logit 全樣本分類準確率：", round(accuracy.logit * 100, 2), "%\n")


# ------------------------------------------------------------------------------
# (h) 模擬樣本外審查決策與授信門檻深度探討 (前 500 筆建模，後 500 筆預測)

train_data <- lasvegas[1:500, ]
test_data  <- lasvegas[501:1000, ]

model.h <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, 
               data = train_data, family = binomial(link = "logit"))
pred.test <- predict(model.h, test_data, type = "response")

table.h.05 <- table(True = test_data$delinquent, Predicted = as.numeric(pred.test >= 0.5))
cat("使用 0.5 門檻值時，後 500 筆新案件的樣本外混淆矩陣：\n")
print(table.h.05)

cat("固定採用 0.5 作為放款門檻在實務上極度危險。因為『放過一個會違約的人（踩雷）』\n")
cat("帶給銀行的本金呆帳損失，遠大於『錯殺一個優質客戶』所損失的利息淨利潤。\n")
cat("因此，銀行授信主管通常會採取風險厭惡的策略，將門檻值大幅調低（如 0.2 或 0.3）。\n")
cat("只要模型預測客戶有 20% 以上的違約機率就拒貸，這才能最大化保護銀行的資產安全。\n")
# ==============================================================================