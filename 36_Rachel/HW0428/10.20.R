library(AER)
# 假設資料集名稱為 capm5
# 建立應變數 (y) 與自變數 (x)
capm5$y <- capm5$msft - capm5$riskfree
capm5$x <- capm5$mkt - capm5$riskfree

#a
cat("\n--- a. OLS 估計 ---\n")
ols_model <- lm(y ~ x, data = capm5)
print(summary(ols_model))
#b
cat("\n--- b. RANK 第一階段迴歸 ---\n")
capm5$RANK <- rank(capm5$x)
stage1_b <- lm(x ~ RANK, data = capm5)
summary(stage1_b)

#c
cat("\n--- c. Hausman 檢定 (RANK) ---\n")
capm5$v_hat <- resid(stage1_b)
aug_model_c <- lm(y ~ x + v_hat, data = capm5)
print(summary(aug_model_c))

#d
cat("\n--- d. IV/2SLS 估計 (RANK) ---\n")
iv_model_d <- ivreg(y ~ x | RANK, data = capm5)
summary(iv_model_d)

#e
# 如果市場超額報酬 (x) 大於 0，則給予 1，否則給予 0
capm5$POS <- ifelse(capm5$x > 0, 1, 0)
stage1_e <- lm(x~ RANK + POS, data = capm5)
summary(stage1_e)

#f
capm5$v_hat_e <- resid(stage1_e)
aug_model_f <- lm(y ~ x + v_hat_e, data = capm5)
summary(aug_model_f)

#g
iv_model_g <- ivreg(y ~ x | RANK + POS, data = capm5)
summary(iv_model_g)

#h
iv_resids <- resid(iv_model_g)
sargan_reg <- lm(iv_resids ~ RANK + POS, data = capm5)
summary_sargan <- summary(sargan_reg)
N <- nobs(sargan_reg) 
R2_sargan <- summary_sargan$r.squared
sargan_stat <- N * R2_sargan
p_val <- 1 - pchisq(sargan_stat, df = 1)
cat("Sargan Statistic (nR^2):", sargan_stat, "\n")
cat("P-value:", p_val, "\n")
