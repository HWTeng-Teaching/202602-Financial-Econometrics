# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 10 - Q18
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)

data("mroz")

mroz_lf <- subset(mroz, lfp == 1)
cat("觀測數：", nrow(mroz_lf), "\n")

# ── 輔助函式：手動 2SLS ──────────────────────────────────────
two_sls <- function(dep_var, endog_name, exog_names, iv_names, data) {
  # 第一階段
  rhs_first <- paste(c(iv_names, exog_names), collapse = " + ")
  f_first   <- as.formula(paste(endog_name, "~", rhs_first))
  stage1    <- lm(f_first, data = data)
  data[[paste0(endog_name, "_hat")]] <- fitted(stage1)

  # 第二階段
  rhs_second <- paste(c(paste0(endog_name, "_hat"), exog_names), collapse = " + ")
  f_second   <- as.formula(paste(dep_var, "~", rhs_second))
  stage2     <- lm(f_second, data = data)

  # 修正標準誤（使用原始殘差）
  rhs_orig <- paste(c(endog_name, exog_names), collapse = " + ")
  f_orig   <- as.formula(paste(dep_var, "~", rhs_orig))
  X_orig   <- model.matrix(f_orig, data = data)
  y        <- data[[dep_var]]
  beta_raw <- coef(stage2)

  # ── 修正：手動設定正確名稱，不用 gsub ──
  correct_names <- c("(Intercept)", endog_name, exog_names)
  beta <- setNames(as.numeric(beta_raw), correct_names)

  resid_true <- as.numeric(y - X_orig %*% beta)
  n   <- nrow(data)
  k   <- length(beta)
  s2  <- sum(resid_true^2) / (n - k)
  X2  <- model.matrix(f_second, data = data)
  vcov_iv <- s2 * solve(t(X2) %*% X2)
  se   <- setNames(sqrt(diag(vcov_iv)), correct_names)
  tval <- beta / se
  pval <- 2 * pt(abs(tval), df = n - k, lower.tail = FALSE)

  result <- data.frame(
    Estimate  = beta,
    Std.Error = se,
    t.value   = tval,
    p.value   = pval,
    row.names = correct_names
  )
  list(coef_table = result, beta = beta, se = se,
       residuals = resid_true, n = n, k = k, s2 = s2,
       stage1 = stage1)
}

# ── 輔助函式：F 統計量 ────────────────────────────────────────
f_stat_zero <- function(lm_obj, var_names) {
  coefs  <- coef(lm_obj)
  vcov_m <- vcov(lm_obj)
  idx    <- match(var_names, names(coefs))
  R_mat  <- diag(length(coefs))[idx, , drop = FALSE]
  Rb_r   <- R_mat %*% coefs
  q      <- length(var_names)
  F_val  <- as.numeric(t(Rb_r) %*% solve(R_mat %*% vcov_m %*% t(R_mat)) %*% Rb_r) / q
  p_val  <- pf(F_val, df1 = q, df2 = df.residual(lm_obj), lower.tail = FALSE)
  cat(sprintf("F 統計量 = %.4f，df1 = %d，df2 = %d，p 值 = %.4f\n",
              F_val, q, df.residual(lm_obj), p_val))
  invisible(list(F = F_val, p = p_val))
}

# ----- (a) -----
mroz_lf$MOTHERCOLL <- as.integer(mroz_lf$mothereduc > 12)
mroz_lf$FATHERCOLL <- as.integer(mroz_lf$fathereduc > 12)
mroz_lf$log_wage   <- log(mroz_lf$wage)
mroz_lf$exper2     <- mroz_lf$exper^2

cat("\n--- (a) ---\n")
cat("MOTHERCOLL = 1 的比例：", round(mean(mroz_lf$MOTHERCOLL) * 100, 2), "%\n")
cat("FATHERCOLL = 1 的比例：", round(mean(mroz_lf$FATHERCOLL) * 100, 2), "%\n")
cat("至少一方有大學教育：",
    round(mean(mroz_lf$MOTHERCOLL | mroz_lf$FATHERCOLL) * 100, 2), "%\n")

# ----- (b) -----
cat("\n--- (b) 相關矩陣 ---\n")
print(round(cor(mroz_lf[, c("educ", "MOTHERCOLL", "FATHERCOLL")]), 4))

# ----- (c) -----
cat("\n--- (c) IV/2SLS（IV = MOTHERCOLL）---\n")
iv_c <- two_sls("log_wage", "educ",
                exog_names = c("exper", "exper2"),
                iv_names   = "MOTHERCOLL",
                data       = mroz_lf)
print(round(iv_c$coef_table, 4))

alpha  <- 0.05
t_crit <- qt(1 - alpha / 2, df = iv_c$n - iv_c$k)
est_c  <- iv_c$beta["educ"]
se_c   <- iv_c$se["educ"]
ci_c   <- c(est_c - t_crit * se_c, est_c + t_crit * se_c)
cat(sprintf("EDUC 的 95%% CI：[%.4f, %.4f]\n", ci_c[1], ci_c[2]))

# ----- (d) -----
cat("\n--- (d) 第一階段迴歸 ---\n")
fs_c <- lm(educ ~ MOTHERCOLL + exper + exper2, data = mroz_lf)
print(summary(fs_c))
cat("F 統計量（MOTHERCOLL = 0）：\n")
f_stat_zero(fs_c, "MOTHERCOLL")

# ----- (e) -----
cat("\n--- (e) IV/2SLS（IV = MOTHERCOLL + FATHERCOLL）---\n")
iv_e <- two_sls("log_wage", "educ",
                exog_names = c("exper", "exper2"),
                iv_names   = c("MOTHERCOLL", "FATHERCOLL"),
                data       = mroz_lf)
print(round(iv_e$coef_table, 4))

t_crit_e <- qt(1 - alpha / 2, df = iv_e$n - iv_e$k)
est_e    <- iv_e$beta["educ"]
se_e     <- iv_e$se["educ"]
ci_e     <- c(est_e - t_crit_e * se_e, est_e + t_crit_e * se_e)
cat(sprintf("EDUC 的 95%% CI：[%.4f, %.4f]\n", ci_e[1], ci_e[2]))
cat(sprintf("CI 寬度比較 — (c): %.4f，(e): %.4f\n",
            ci_c[2] - ci_c[1], ci_e[2] - ci_e[1]))

# ----- (f) -----
cat("\n--- (f) 第一階段迴歸（兩個 IV）---\n")
fs_e <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + exper2, data = mroz_lf)
print(summary(fs_e))
cat("聯合 F 統計量（MOTHERCOLL = FATHERCOLL = 0）：\n")
f_stat_zero(fs_e, c("MOTHERCOLL", "FATHERCOLL"))

# ----- (g) -----
cat("\n--- (g) Sargan 過度識別檢驗 ---\n")
sargan_reg  <- lm(iv_e$residuals ~ MOTHERCOLL + FATHERCOLL + exper + exper2,
                  data = mroz_lf)
n           <- iv_e$n
r2_s        <- summary(sargan_reg)$r.squared
sargan_stat <- n * r2_s
df_s        <- 1
p_s         <- pchisq(sargan_stat, df = df_s, lower.tail = FALSE)
cat(sprintf("Sargan 統計量 = %.4f，df = %d，p 值 = %.4f\n",
            sargan_stat, df_s, p_s))

cat("\n===== Q18 完成 =====\n")
