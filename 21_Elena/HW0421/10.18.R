# ============================================================
# HW6 - 10.18  工具變數估計 (mroz 資料集)
#
# 參考模型：Example 10.5 (EX10.5.png)
#   ln(WAGE) = β1 + β2*EDUC + β3*EXPER + β4*EXPER²
#   樣本：mroz 中 lfp == 1 的 428 筆已婚女性
#   工具變數：原例使用 MOTHEREDUC/FATHEREDUC；
#             本題改用大學學歷虛擬變數 MOTHERCOLL/FATHERCOLL
# ============================================================

# 載入必要套件
install.packages("AER")   # 提供 ivreg()
library(AER)
library(car)              # 提供 linearHypothesis()

# 載入資料（使用絕對路徑）
setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("mroz.rdata")

# 限制樣本為有勞動力參與的已婚女性 (lfp == 1，共 428 筆)
mroz1 <- mroz[mroz$lfp == 1, ]


# ============================================================
# (a) 建立虛擬變數 MOTHERCOLL 與 FATHERCOLL，計算父母大學學歷比例

mroz1$MOTHERCOLL <- as.integer(mroz1$mothereduc > 12)
mroz1$FATHERCOLL <- as.integer(mroz1$fathereduc > 12)

cat(sprintf("母親有大學學歷 (MOTHERCOLL=1) 的比例：%.2f%%\n",
            mean(mroz1$MOTHERCOLL) * 100))
cat(sprintf("父親有大學學歷 (FATHERCOLL=1) 的比例：%.2f%%\n",
            mean(mroz1$FATHERCOLL) * 100))
cat(sprintf("父母至少一方有大學學歷的比例：%.2f%%\n",
            mean(mroz1$MOTHERCOLL == 1 | mroz1$FATHERCOLL == 1) * 100))


# ============================================================
# (b) EDUC、MOTHERCOLL、FATHERCOLL 的相關係數

cor_matrix <- cor(mroz1[, c("educ", "MOTHERCOLL", "FATHERCOLL",
                             "mothereduc", "fathereduc")])
cat("相關係數矩陣：\n")
print(round(cor_matrix, 4))

cat("\n[結論]\n")
cat("MOTHERCOLL/FATHERCOLL 為二元虛擬變數，只捕捉「是否受過大學教育」的門檻效果，\n")
cat("可避免原始教育年數在高值時的衡量誤差，因此可能是更乾淨的工具變數。\n")


# ============================================================
# (c) IV 估計（Example 10.5 模型，工具變數：MOTHERCOLL）

# 語法：應變數 ~ 內生+外生變數 | 外生變數+工具變數
mroz1.iv.c <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                      exper + I(exper^2) + MOTHERCOLL,
                    data = mroz1)
print(summary(mroz1.iv.c))

ci_c <- confint(mroz1.iv.c, level = 0.95)
cat("\nEDUC 係數的 95% 信賴區間：\n")
print(ci_c["educ", ])


# ============================================================
# (d) (c) 的第一階段方程式與工具變數強度 F 檢定

first_stage_d <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = mroz1)
print(summary(first_stage_d))

f_test_d <- linearHypothesis(first_stage_d, "MOTHERCOLL = 0")
cat("\nF 檢定（H0: MOTHERCOLL 對 EDUC 無影響）：\n")
print(f_test_d)

f_val_d <- f_test_d$F[2]
cat(sprintf("\nF 統計量 = %.4f\n", f_val_d))
if (f_val_d > 10) {
  cat("-> F > 10，MOTHERCOLL 為強工具變數。\n")
} else {
  cat("-> F < 10，MOTHERCOLL 可能為弱工具變數。\n")
}


# ============================================================
# (e) IV 估計（Example 10.5 模型，工具變數：MOTHERCOLL + FATHERCOLL）

mroz1.iv.e <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                      exper + I(exper^2) + MOTHERCOLL + FATHERCOLL,
                    data = mroz1)
print(summary(mroz1.iv.e))

ci_e <- confint(mroz1.iv.e, level = 0.95)
cat("\nEDUC 係數的 95% 信賴區間：\n")
print(ci_e["educ", ])

# 比較信賴區間寬度
width_c <- ci_c["educ", 2] - ci_c["educ", 1]
width_e <- ci_e["educ", 2] - ci_e["educ", 1]
cat(sprintf("\n(c) 信賴區間寬度 = %.4f\n", width_c))
cat(sprintf("(e) 信賴區間寬度 = %.4f\n", width_e))
if (width_e < width_c) {
  cat("-> (e) 的信賴區間較窄，兩個工具變數提升了估計精確度。\n")
} else {
  cat("-> (e) 的信賴區間較寬。\n")
}


# ============================================================
# (f) (e) 的第一階段方程式與聯合 F 檢定

first_stage_f <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL,
                    data = mroz1)
print(summary(first_stage_f))

f_test_f <- linearHypothesis(first_stage_f,
                              c("MOTHERCOLL = 0", "FATHERCOLL = 0"))
cat("\n聯合 F 檢定（H0: MOTHERCOLL 與 FATHERCOLL 同時對 EDUC 無影響）：\n")
print(f_test_f)

f_val_f <- f_test_f$F[2]
cat(sprintf("\n聯合 F 統計量 = %.4f\n", f_val_f))
if (f_val_f > 10) {
  cat("-> F > 10，工具變數組合足夠強。\n")
} else {
  cat("-> F < 10，工具變數組合可能不夠強。\n")
}


# ============================================================
# (g) Sargan 過度識別檢定（多餘工具變數有效性）

# diagnostics=TRUE 同時輸出：Weak instruments / Wu-Hausman / Sargan 三項檢定
print(summary(mroz1.iv.e, diagnostics = TRUE))

cat("\n[Sargan 檢定解讀]\n")
cat("H0：多餘工具變數（FATHERCOLL）與誤差項無相關（工具變數外生有效）\n")
cat("p-value > 0.05 → 不拒絕 H0，FATHERCOLL 通過外生性檢定，工具變數設定有效。\n")
cat("p-value < 0.05 → 拒絕 H0，工具變數可能與誤差項相關，設定有問題。\n")
