# 載入必要套件
library(AER)    # ivreg()
library(car)    # linearHypothesis()

# 載入資料
setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("capm5.rdata")

# 計算超額報酬（excess returns）
capm5$excess_msft <- capm5$msft - capm5$riskfree   # Microsoft 超額報酬
capm5$excess_mkt  <- capm5$mkt  - capm5$riskfree   # 市場超額報酬


# ============================================================
# (a) OLS 估計 CAPM 模型

capm.ols <- lm(excess_msft ~ excess_mkt, data = capm5)
print(summary(capm.ols))

beta_ols <- coef(capm.ols)["excess_mkt"]
cat(sprintf("\nβ (OLS) = %.4f\n", beta_ols))
if (beta_ols > 1) {
  cat("-> β > 1，Microsoft 為高風險積極型股票（比市場波動更大）。\n")
} else {
  cat("-> β < 1，Microsoft 為相對安全防禦型股票（比市場波動更小）。\n")
}


# ============================================================
# (b) 建立 RANK 工具變數，估計第一階段迴歸
# ============================================================
cat("\n======================================================\n")
cat("(b) 建立 RANK 工具變數與第一階段迴歸\n")
cat("======================================================\n")

# 將 (r_m - r_f) 由小到大排序，給予排名 1~180
capm5$RANK <- rank(capm5$excess_mkt)

cat("RANK 變數摘要：\n")
print(summary(capm5$RANK))

# 第一階段迴歸：(r_m - r_f) ~ RANK
first_stage_b <- lm(excess_mkt ~ RANK, data = capm5)
print(summary(first_stage_b))

r2_b <- summary(first_stage_b)$r.squared
f_b  <- summary(first_stage_b)$fstatistic[1]
cat(sprintf("\n第一階段 R² = %.4f\n", r2_b))
cat(sprintf("F 統計量    = %.4f\n", f_b))
if (f_b > 10) {
  cat("-> F > 10，RANK 為強工具變數。\n")
} else {
  cat("-> F < 10，RANK 可能為弱工具變數。\n")
}


# ============================================================
# (c) Hausman 內生性檢定（手動）：加入第一階段殘差
# ============================================================
cat("\n======================================================\n")
cat("(c) Hausman 內生性檢定（第一階段殘差法）\n")
cat("     使用 RANK 的第一階段殘差\n")
cat("======================================================\n")

# 取得第一階段殘差 v_hat
capm5$v_hat_c <- residuals(first_stage_b)

# 擴增迴歸：將 v_hat 加入原始 CAPM 模型
augmented_c <- lm(excess_msft ~ excess_mkt + v_hat_c, data = capm5)
print(summary(augmented_c))

pval_vhat_c <- summary(augmented_c)$coefficients["v_hat_c", "Pr(>|t|)"]
cat(sprintf("\nv_hat 的 p-value = %.4f\n", pval_vhat_c))
if (pval_vhat_c < 0.01) {
  cat("-> 在 1% 顯著水準下拒絕 H0，市場超額報酬具有內生性，OLS 估計有偏誤。\n")
} else {
  cat("-> 在 1% 顯著水準下不拒絕 H0，無法確認市場超額報酬的內生性。\n")
}


# ============================================================
# (d) IV/2SLS 估計（工具變數：RANK）
# ============================================================
cat("\n======================================================\n")
cat("(d) IV/2SLS 估計（工具變數：RANK）\n")
cat("======================================================\n")

capm.iv.d <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm5)
print(summary(capm.iv.d))

beta_iv_d <- coef(capm.iv.d)["excess_mkt"]
cat(sprintf("\nβ (OLS) = %.4f\n", beta_ols))
cat(sprintf("β (IV)  = %.4f\n", beta_iv_d))
cat("-> 比較：IV 估計是否與 OLS 接近？（若市場報酬測量誤差嚴重，兩者差異應明顯）\n")


# ============================================================
# (e) 建立 POS 工具變數，雙 IV 第一階段與聯合 F 檢定
# ============================================================
cat("\n======================================================\n")
cat("(e) 建立 POS 工具變數，第一階段聯合 F 檢定\n")
cat("======================================================\n")

# POS = 1 若市場超額報酬 > 0，否則為 0
capm5$POS <- as.integer(capm5$excess_mkt > 0)

cat(sprintf("POS = 1（市場正報酬月份）的比例：%.2f%%\n",
            mean(capm5$POS) * 100))

# 第一階段迴歸：(r_m - r_f) ~ RANK + POS
first_stage_e <- lm(excess_mkt ~ RANK + POS, data = capm5)
print(summary(first_stage_e))

# 聯合 F 檢定：RANK 與 POS 同時為 0
f_test_e <- linearHypothesis(first_stage_e, c("RANK = 0", "POS = 0"))
cat("\n聯合 F 檢定（H0: RANK 與 POS 同時對市場超額報酬無影響）：\n")
print(f_test_e)

r2_e   <- summary(first_stage_e)$r.squared
f_val_e <- f_test_e$F[2]
cat(sprintf("\n聯合 F 統計量 = %.4f\n", f_val_e))
cat(sprintf("第一階段 R²   = %.4f\n", r2_e))
if (f_val_e > 10) {
  cat("-> F > 10，工具變數組合足夠強。\n")
} else {
  cat("-> F < 10，工具變數組合可能不夠強。\n")
}


# ============================================================
# (f) Hausman 內生性檢定（使用 (e) 的第一階段殘差）
# ============================================================
cat("\n======================================================\n")
cat("(f) Hausman 內生性檢定（使用 RANK + POS 的第一階段殘差）\n")
cat("======================================================\n")

# 取得 (e) 第一階段殘差
capm5$v_hat_e <- residuals(first_stage_e)

# 擴增迴歸
augmented_f <- lm(excess_msft ~ excess_mkt + v_hat_e, data = capm5)
print(summary(augmented_f))

pval_vhat_f <- summary(augmented_f)$coefficients["v_hat_e", "Pr(>|t|)"]
cat(sprintf("\nv_hat 的 p-value = %.4f\n", pval_vhat_f))
if (pval_vhat_f < 0.01) {
  cat("-> 在 1% 顯著水準下拒絕 H0，確認市場超額報酬具有內生性。\n")
} else {
  cat("-> 在 1% 顯著水準下不拒絕 H0，無法確認內生性。\n")
}


# ============================================================
# (g) IV/2SLS 估計（工具變數：RANK + POS）
# ============================================================
cat("\n======================================================\n")
cat("(g) IV/2SLS 估計（工具變數：RANK + POS）\n")
cat("======================================================\n")

capm.iv.g <- ivreg(excess_msft ~ excess_mkt | RANK + POS, data = capm5)
print(summary(capm.iv.g))

beta_iv_g <- coef(capm.iv.g)["excess_mkt"]
cat(sprintf("\nβ (OLS)      = %.4f\n", beta_ols))
cat(sprintf("β (IV, RANK) = %.4f\n", beta_iv_d))
cat(sprintf("β (IV, RANK+POS) = %.4f\n", beta_iv_g))


# ============================================================
# (h) 手動 Sargan 過度識別檢定（不使用自動指令）
# ============================================================
cat("\n======================================================\n")
cat("(h) 手動 Sargan 過度識別檢定（5% 顯著水準）\n")
cat("======================================================\n")

# 步驟一：取得 (g) 的 IV/2SLS 殘差
e_hat <- residuals(capm.iv.g)
n     <- length(e_hat)

# 步驟二：將 IV 殘差對所有工具變數（RANK, POS）與截距項迴歸
sargan_reg <- lm(e_hat ~ RANK + POS, data = capm5)
r2_sargan  <- summary(sargan_reg)$r.squared

# 步驟三：計算 Sargan 統計量 = n * R²
sargan_stat <- n * r2_sargan

# 步驟四：自由度 = 多餘工具變數個數 = 工具變數數 - 內生變數數 = 2 - 1 = 1
df_sargan <- 1
chi_critical <- qchisq(0.95, df_sargan)
pval_sargan  <- 1 - pchisq(sargan_stat, df_sargan)

cat(sprintf("樣本數 n               = %d\n", n))
cat(sprintf("殘差迴歸 R²            = %.6f\n", r2_sargan))
cat(sprintf("Sargan 統計量 (n*R²)   = %.4f\n", sargan_stat))
cat(sprintf("自由度                 = %d（多餘工具變數數）\n", df_sargan))
cat(sprintf("χ² 臨界值 (5%%, df=1)  = %.4f\n", chi_critical))
cat(sprintf("p-value                = %.4f\n", pval_sargan))

cat("\n[結論]\n")
if (sargan_stat > chi_critical) {
  cat("-> 拒絕 H0，多餘工具變數（POS）可能與誤差項相關，工具變數設定有問題。\n")
} else {
  cat("-> 不拒絕 H0，POS 通過外生性檢定，工具變數設定有效。\n")
}

