#10.18

rm(list=ls()) #Removes all items in Environment!

library(AER)
library(POE5Rdata)

data("mroz", package="POE5Rdata")
mroz1 <- mroz[mroz$lfp==1,] #restricts sample to lfp=1

head(mroz1)

#a

mroz1$MOTHERCOLL <- ifelse(mroz1$mothereduc > 12, 1, 0)
mroz1$FATHERCOLL <- ifelse(mroz1$fathereduc > 12, 1, 0)

cat("母親受過部分大學教育的比例：", mean(mroz1$MOTHERCOLL) * 100, "%\n")
cat("父親受過部分大學教育的比例：", mean(mroz1$FATHERCOLL) * 100, "%\n")

#b
cor_matrix <- cor(mroz1[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print(cor_matrix)

#c
iv_model_c <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                      MOTHERCOLL + exper + I(exper^2), data = mroz1)
summary(iv_model_c)
confint(iv_model_c, "educ", level = 0.95)

#d
first_stage_d <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = mroz1)
summary(first_stage_d)
# 執行 F-test 檢定 MOTHERCOLL 的係數是否為 0
linearHypothesis(first_stage_d, "MOTHERCOLL = 0")

#e
iv_model_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, 
                    data = mroz1)
summary(iv_model_e)
confint(iv_model_e, "educ", level = 0.95)

#f
first_stage_f <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz1)
summary(first_stage_f)

# 檢定 H0: MOTHERCOLL = 0 且 FATHERCOLL = 0
linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#g
summary(iv_model_e, diagnostics = TRUE)


#10.20
library(AER)
library(POE5Rdata)

data("capm5", package="POE5Rdata")

head(capm5)

# 計算微軟與市場的風險溢酬 (Risk Premium)
capm5$msft_riskpremium <- capm5$msft - capm5$riskfree  # r_j - r_f
capm5$mkt_riskpremium <- capm5$mkt - capm5$riskfree    # r_m - r_f

#a
model_ols <- lm(msft_riskpremium ~ mkt_riskpremium, data = capm5)
summary(model_ols)

#b
capm5$RANK <- rank(capm5$mkt_riskpremium)
# 第一階段回歸：將內生變數對工具變數回歸
first_stage_b <- lm(mkt_riskpremium ~ RANK, data = capm5)
summary(first_stage_b)

#c
capm5$v_hat <- resid(first_stage_b)
model_augmented <- lm(msft_riskpremium ~ mkt_riskpremium + v_hat, data = capm5)
summary(model_augmented)

#d
model_iv_d <- ivreg(msft_riskpremium ~ mkt_riskpremium | RANK, data = capm5)
summary(model_iv_d)

#e
capm5$POS <- ifelse(capm5$mkt_riskpremium > 0, 1, 0)
first_stage_e <- lm(mkt_riskpremium ~ RANK + POS, data = capm5)
summary(first_stage_e)
# 講解：檢定 RANK 與 POS 的聯合顯著性 (F-test)。
linearHypothesis(first_stage_e, c("RANK=0", "POS=0"))

#f
capm5$v_hat_e <- resid(first_stage_e)
model_hausman <- lm(msft_riskpremium ~ mkt_riskpremium + v_hat_e, data = capm5)
summary(model_hausman)

#g
model_iv_g <- ivreg(msft_riskpremium ~ mkt_riskpremium | RANK + POS, data = capm5)
summary(model_iv_g)

#h
capm5$e_hat_iv <- resid(model_iv_g)

# 2. 將殘差對所有的工具變數 (外生變數) 進行迴歸
sargan_aux_reg <- lm(e_hat_iv ~ RANK + POS, data = capm5)

# 3. 取得輔助迴歸的 R-squared
R2_aux <- summary(sargan_aux_reg)$r.squared

# 4. 計算 Sargan 檢定統計量 N * R^2
N <- nrow(capm5)
sargan_stat <- N * R2_aux

# 5. 計算 p-value (自由度 = 總IV數 - 內生變數數 = 2 - 1 = 1)
p_value_sargan <- 1 - pchisq(sargan_stat, df = 1)

cat("Sargan Test Statistic:", sargan_stat, "\n")
cat("P-value:", p_value_sargan, "\n")

# 結論：若 p-value > 0.05，則可以認為工具變數具有有效性。
#自動化檢驗 summary(model_iv_g, diagnostics = TRUE)
