# 財計經作業 6 - 第十章：隨機解釋變數與工具變數法 (IV/2SLS)
# 題目來源：hw6_ch10題目.docx (10.18, 10.20)

# 載入必要套件
# 若尚未安裝，請先執行：install.packages(c("POE5Rdata", "AER", "sandwich", "lmtest"))
library(POE5Rdata)
library(AER)      # 提供 ivreg() 函數與診斷檢定
library(sandwich) # 穩健標準誤
library(lmtest)   # 假設檢定工具

# =========================================================
# Question 10.18 - 工資方程式與父母教育工具變數
# =========================================================
cat("\n--- Question 10.18 ---\n")
data("mroz")
# 篩選參加勞動市場的已婚婦女 (428 筆觀察值)
mroz1 <- subset(mroz, lfp == 1)

# a. 建立虛擬變數 MOTHERCOLL 與 FATHERCOLL (教育年數 > 12)
mroz1$MOTHERCOLL <- as.numeric(mroz1$mothereduc > 12)
mroz1$FATHERCOLL <- as.numeric(mroz1$fathereduc > 12)
pct_mother <- mean(mroz1$MOTHERCOLL) * 100
pct_father <- mean(mroz1$FATHERCOLL) * 100
cat("10.18a: 父母大學教育百分比\n")
cat("母親: ", round(pct_mother, 2), "%\n")
cat("父親: ", round(pct_father, 2), "%\n\n")

# b. 計算相關係數
cor_matrix <- cor(mroz1[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
cat("10.18b: 相關係數矩陣\n")
print(round(cor_matrix, 4))
cat("\n")

# c. 使用 MOTHERCOLL 作為工具變數估計工資方程式
iv_c <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + MOTHERCOLL, data = mroz1)
conf_c <- confint(iv_c, "educ", level = 0.95)
cat("10.18c: 僅使用 MOTHERCOLL 時 EDUC 的 95% 信賴區間\n")
print(conf_c)
cat("\n")

# d. 第一階段回歸與工具變數強度檢定 (F-test)
fs_d <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = mroz1)
cat("10.18d: 第一階段 F 檢定 (MOTHERCOLL 強度)\n")
print(linearHypothesis(fs_d, "MOTHERCOLL = 0"))
cat("\n")

# e. 同時使用 MOTHERCOLL 與 FATHERCOLL 作為工具變數
iv_e <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz1)
conf_e <- confint(iv_e, "educ", level = 0.95)
cat("10.18e: 使用兩個工具變數時 EDUC 的 95% 信賴區間\n")
print(conf_e)
cat("\n")

# f. 第一階段聯合顯著性檢定
fs_f <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz1)
cat("10.18f: 第一階段 F 檢定 (聯合強度)\n")
print(linearHypothesis(fs_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0")))
cat("\n")

# g. 診斷檢定 (Weak instruments, Wu-Hausman, Sargan)
cat("10.18g: 診斷檢定結果 (含 Sargan 過度識別檢定)\n")
print(summary(iv_e, diagnostics = TRUE))
cat("\n\n")


# =========================================================
# Question 10.20 - CAPM 模型與測量誤差 (RANK 工具變數)
# =========================================================
cat("--- Question 10.20 ---\n")
data("capm5")
# 計算超額報酬 (y: 微軟, x: 市場)
capm5$y <- capm5$msft - capm5$riskfree
capm5$x <- capm5$mkt - capm5$riskfree

# a. OLS 估計 CAPM
ols_a <- lm(y ~ x, data = capm5)
cat("10.20a: OLS 估計結果\n")
print(summary(ols_a))
cat("\n")

# b. 建立排序工具變數 RANK 並檢定強度
capm5$RANK <- rank(capm5$x)
fs_b <- lm(x ~ RANK, data = capm5)
cat("10.20b: 第一階段結果 (RANK 強度)\n")
print(summary(fs_b))
cat("\n")

# c. Hausman 檢定 (人為回歸法)
v_hat <- residuals(fs_b)
aug_c <- lm(y ~ x + v_hat, data = capm5)
cat("10.20c: Hausman 檢定 (檢定 v_hat 之顯著性)\n")
print(summary(aug_c))
cat("\n")

# d. 使用 RANK 作為 IV 進行估計
iv_d <- ivreg(y ~ x | RANK, data = capm5)
cat("10.20d: IV/2SLS 估計結果 (使用 RANK)\n")
print(summary(iv_d))
cat("\n")

# e. 增加 POS (市場回報正負) 作為第二個工具變數
capm5$POS <- as.numeric(capm5$x > 0)
fs_e <- lm(x ~ RANK + POS, data = capm5)
cat("10.20e: 第一階段聯合檢定 (RANK + POS)\n")
print(linearHypothesis(fs_e, c("RANK = 0", "POS = 0")))
cat("\n")

# f. Hausman 檢定 (多重 IV 版本)
v_hat_e <- residuals(fs_e)
aug_f <- lm(y ~ x + v_hat_e, data = capm5)
cat("10.20f: Hausman 檢定 (使用 RANK 與 POS)\n")
print(summary(aug_f))
cat("\n")

# g. IV/2SLS 估計結果 (雙工具變數)
iv_g <- ivreg(y ~ x | RANK + POS, data = capm5)
cat("10.20g: IV/2SLS 聯合估計結果\n")
print(summary(iv_g))
cat("\n")

# h. Sargan 過度識別檢定 (手動計算)
e_iv <- residuals(iv_g)
sargan_reg <- lm(e_iv ~ RANK + POS, data = capm5)
n <- nrow(capm5)
sargan_stat <- n * summary(sargan_reg)$r.squared
p_val_sargan <- 1 - pchisq(sargan_stat, df = 1) # df = L - B = 2 - 1 = 1
cat("10.20h: Sargan 檢定結果\n")
cat("Sargan 統計量: ", round(sargan_stat, 4), "\n")
cat("p-value: ", round(p_val_sargan, 4), "\n")
