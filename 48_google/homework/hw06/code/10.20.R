# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 10 - Q20
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)

data("capm5")
cat("欄位名稱：", names(capm5), "\n")
cat("觀測數：", nrow(capm5), "\n")

# 建立超額報酬
capm5$excess_msft   <- capm5$msft - capm5$riskfree
capm5$excess_market <- capm5$mkt  - capm5$riskfree

# ── 輔助函式：手動 2SLS（簡單單一內生變數版）─────────────────
two_sls_simple <- function(dep, endog, exog, ivs, data) {
  # 第一階段
  f1     <- as.formula(paste(endog, "~", paste(c(ivs, exog), collapse = " + ")))
  stage1 <- lm(f1, data = data)
  data[[paste0(endog, "_hat")]] <- fitted(stage1)

  # 第二階段
  rhs2   <- paste(c(paste0(endog, "_hat"), exog), collapse = " + ")
  f2     <- as.formula(paste(dep, "~", rhs2))
  stage2 <- lm(f2, data = data)

  # 修正殘差與標準誤
  rhs_orig <- paste(c(endog, exog), collapse = " + ")
  f_orig   <- as.formula(paste(dep, "~", rhs_orig))
  X_orig   <- model.matrix(f_orig, data = data)
  y        <- data[[dep]]
  beta     <- coef(stage2)
  names(beta) <- gsub(paste0(endog, "_hat"), endog, names(beta))
  resid_true  <- as.numeric(y - X_orig %*% beta)
  n  <- nrow(data); k <- length(beta)
  s2 <- sum(resid_true^2) / (n - k)
  X2 <- model.matrix(f2, data = data)
  se   <- sqrt(diag(s2 * solve(t(X2) %*% X2)))
  tval <- beta / se
  pval <- 2 * pt(abs(tval), df = n - k, lower.tail = FALSE)

  result <- data.frame(Estimate  = beta,
                       Std.Error = se,
                       t.value   = tval,
                       p.value   = pval,
                       row.names = names(beta))
  list(coef_table = result, beta = beta, se = se,
       residuals = resid_true, n = n, k = k, stage1 = stage1)
}

# ── 輔助函式：聯合 F 統計量 ───────────────────────────────────
f_stat_zero <- function(lm_obj, var_names) {
  coefs  <- coef(lm_obj)
  vcov_m <- vcov(lm_obj)
  idx    <- match(var_names, names(coefs))
  R_mat  <- diag(length(coefs))[idx, , drop = FALSE]
  Rb_r   <- R_mat %*% coefs
  q      <- length(var_names)
  F_val  <- as.numeric(t(Rb_r) %*%
               solve(R_mat %*% vcov_m %*% t(R_mat)) %*% Rb_r) / q
  p_val  <- pf(F_val, df1 = q, df2 = df.residual(lm_obj), lower.tail = FALSE)
  cat(sprintf("F 統計量 = %.4f，df1 = %d，df2 = %d，p 值 = %.4f\n",
              F_val, q, df.residual(lm_obj), p_val))
  invisible(list(F = F_val, p = p_val))
}

# ----- (a) OLS -----
cat("\n--- (a) OLS 估計 CAPM ---\n")
ols_a <- lm(excess_msft ~ excess_market, data = capm5)
print(summary(ols_a))
cat(sprintf("OLS beta = %.4f\n", coef(ols_a)["excess_market"]))

# ----- (b) RANK，第一階段迴歸 -----
cat("\n--- (b) RANK 第一階段迴歸 ---\n")
capm5$RANK <- rank(capm5$excess_market)
first_b    <- lm(excess_market ~ RANK, data = capm5)
print(summary(first_b))
cat(sprintf("第一階段 R² = %.4f\n", summary(first_b)$r.squared))

# ----- (c) Hausman 內生性檢驗 -----
cat("\n--- (c) Hausman 檢驗（加入第一階段殘差 v_hat）---\n")
capm5$v_hat_b <- residuals(first_b)
hausman_c     <- lm(excess_msft ~ excess_market + v_hat_b, data = capm5)
print(summary(hausman_c))

# ----- (d) IV/2SLS（IV = RANK）-----
cat("\n--- (d) IV/2SLS（IV = RANK）---\n")
iv_d <- two_sls_simple("excess_msft", "excess_market",
                        exog = character(0), ivs = "RANK",
                        data = capm5)
print(round(iv_d$coef_table, 4))

# ----- (e) RANK + POS，第一階段迴歸 -----
cat("\n--- (e) 建立 POS，第一階段迴歸（RANK + POS）---\n")
capm5$POS <- as.integer(capm5$excess_market > 0)
first_e   <- lm(excess_market ~ RANK + POS, data = capm5)
print(summary(first_e))
cat(sprintf("第一階段 R² = %.4f\n", summary(first_e)$r.squared))
cat("聯合 F 統計量（RANK = POS = 0）：\n")
f_stat_zero(first_e, c("RANK", "POS"))

# ----- (f) Hausman 檢驗（使用 (e) 殘差）-----
cat("\n--- (f) Hausman 檢驗（v_hat 來自 (e)）---\n")
capm5$v_hat_e <- residuals(first_e)
hausman_f     <- lm(excess_msft ~ excess_market + v_hat_e, data = capm5)
print(summary(hausman_f))

# ----- (g) IV/2SLS（IV = RANK + POS）-----
cat("\n--- (g) IV/2SLS（IV = RANK + POS）---\n")
iv_g <- two_sls_simple("excess_msft", "excess_market",
                        exog = character(0), ivs = c("RANK", "POS"),
                        data = capm5)
print(round(iv_g$coef_table, 4))

# ----- (h) Sargan 過度識別檢驗（手動）-----
cat("\n--- (h) Sargan 過度識別檢驗（手動）---\n")
sargan_reg_g <- lm(iv_g$residuals ~ RANK + POS, data = capm5)
n_g          <- iv_g$n
r2_g         <- summary(sargan_reg_g)$r.squared
sargan_g     <- n_g * r2_g
df_s         <- 1   # 2 IV - 1 內生變數
p_s          <- pchisq(sargan_g, df = df_s, lower.tail = FALSE)
cat(sprintf("Sargan 統計量 = %.4f，df = %d，p 值 = %.4f\n",
            sargan_g, df_s, p_s))

cat("\n===== Q20 完成 =====\n")
