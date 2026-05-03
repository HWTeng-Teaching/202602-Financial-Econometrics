install.packages(c("wooldridge", "AER", "car"))
library(wooldridge) # 取得 mroz 資料集
library(AER)        # 提供 ivreg 函數進行工具變數回歸
library(car)        # 提供 linearHypothesis 進行 F 檢定
library(stargazer) 

# 篩選出參與勞動市場的 428 位女性 (inlf == 1 代表在勞動力市場中)
mroz1 <- mroz[mroz$lfp==1,] 

mroz1$lwage <- log(mroz1$wage)
mroz1$expersq <- mroz1$exper^2

#a
# 建立虛擬變數 (如果受教年數 > 12，則為 1，否則為 0)
mroz1$MOTHERCOLL <- ifelse(mroz1$mothereduc > 12, 1, 0)
mroz1$FATHERCOLL <- ifelse(mroz1$fathereduc > 12, 1, 0)
# 計算比例 (乘以 100 轉為百分比)
pct_mother <- mean(mroz1$MOTHERCOLL) * 100
pct_father <- mean(mroz1$FATHERCOLL) * 100
cat("母親受過大學以上教育的比例:", round(pct_mother, 2), "%\n")
cat("父親受過大學以上教育的比例:", round(pct_father, 2), "%\n")

#b
cor_matrix <- cor(df[, c("educ", "MOTHERCOLL", "FATHERCOLL")])

cat("--- b. 變數間的相關係數矩陣 ---\n")
print(cor_matrix)
cat("\n")

#c
mroz1.iv <- ivreg(lwage~educ+exper+expersq| exper+expersq+MOTHERCOLL, data=mroz1)
summary(mroz1.iv)
cat("c. 2SLS (MOTHERCOLL) EDUC 估計係數:\n")
print(coef(mroz1.iv)["educ"])
cat("c. EDUC 係數的 95% 信賴區間:\n")
print(confint(mroz1.iv, "educ", level = 0.95))
cat("\n")

# d. 單一 IV 的第一階段 F 檢定
stage1_d <- lm(educ ~ MOTHERCOLL + exper + expersq, data = mroz1)
cat("d. 第一階段 F 檢定 (MOTHERCOLL = 0):\n")
print(linearHypothesis(stage1_d, "MOTHERCOLL = 0"))
cat("\n")

#e
mroz1.iv1 <- ivreg(lwage~educ+exper+expersq| exper+expersq+MOTHERCOLL+FATHERCOLL, data=mroz1)
cat("e. 2SLS (MOTHERCOLL & FATHERCOLL) EDUC 估計係數:\n")
print(coef(mroz1.iv1)["educ"])
cat("e. EDUC 係數的 95% 信賴區間:\n")
print(confint(mroz1.iv1, "educ", level = 0.95))
cat("\n")
summary(mroz1.iv1)

# f
stage1_f <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + expersq , data = mroz1)
cat("f. 第一階段聯合 F 檢定 (MOTHERCOLL=0 & FATHERCOLL=0):\n")
print(linearHypothesis(stage1_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0")))
cat("\n")

#g
cat("g. Sargan Test (Overidentifying restrictions):\n")
print(summary(mroz1.iv1, diagnostics = TRUE)$diagnostics)

summary(mroz1.iv1, diagnostics=TRUE)
