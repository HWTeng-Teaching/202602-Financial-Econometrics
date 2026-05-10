rm(list=ls())
library(POE5Rdata)
library(stargazer)
# stargazer(results, summary=FALSE, type="latex", 
#           title="Simulation Results", 
#           header=FALSE)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(sandwich)
library(AER)


#18
data(mroz)
?mroz
df_work <- subset(mroz, lfp == 1)

#A
df_work$MOTHERCOLL <- ifelse(df_work$mothereduc > 12, 1, 0)
df_work$FATHERCOLL <- ifelse(df_work$fathereduc > 12, 1, 0)

m_pct <- mean(df_work$MOTHERCOLL) * 100
f_pct <- mean(df_work$FATHERCOLL) * 100
cat("a. 父母大學教育比例:\n", "母親:", round(m_pct, 2), "%, 父親:", round(f_pct, 2), "%\n\n")

#B
cor_matrix <- cor(df_work[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print("b. 相關係數矩陣:")
print(round(cor_matrix, 4))
stargazer(cor_matrix, summary=FALSE, type="latex", 
                    header=FALSE)

# C
iv_mod_c <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                    MOTHERCOLL + exper + I(exper^2), data = df_work)

cat("\nc. EDUC 係數估計 (單一 IV):\n")
print(summary(iv_mod_c)$coefficients["educ", ])
cat("95% 信賴區間:\n")
print(confint(iv_mod_c, "educ"))
stargazer(iv_mod_c, summary=FALSE, type="latex", 
          header=FALSE)

# D
first_stage_c <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = df_work)
f_val_c <- linearHypothesis(first_stage_c, "MOTHERCOLL = 0")
cat("\nd. 第一階段 MOTHERCOLL 的 F 檢定值:", round(f_val_c$F[2], 4), "\n")

# E
iv_mod_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                    MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = df_work)

cat("\ne. EDUC 係數估計 (雙 IV):\n")
print(summary(iv_mod_e)$coefficients["educ", ])
cat("95% 信賴區間:\n")
print(confint(iv_mod_e, "educ"))

# F
first_stage_e <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = df_work)
f_val_e <- linearHypothesis(first_stage_e, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))
cat("\nf. 第一階段聯合 F 檢定值:", round(f_val_e$F[2], 4), "\n")

# G
cat("\ng. 過度識別檢定 (Sargan Test):\n")
# diagnostics = TRUE 會顯示 Wu-Hausman 與 Sargan 檢定
summary(iv_mod_e, diagnostics = TRUE)

# 20
data(capm5)
?capm5
capm5$y <- capm5$msft - capm5$riskfree
capm5$x <- capm5$mkt - capm5$riskfree

# A
model_a <- lm(y ~ x, data = capm5)
summary(model_a)

# B
capm5$RANK <- rank(capm5$x)
model_b_stage1 <- lm(x ~ RANK, data = capm5)
summary(model_b_stage1)

# C
v_hat <- residuals(model_b_stage1)
model_c_test <- lm(y ~ x + v_hat, data = capm5)
summary(model_c_test)

# D
model_d_iv <- ivreg(y ~ x | RANK, data = capm5)
summary(model_d_iv)

# E
capm5$POS <- ifelse(capm5$x > 0, 1, 0)
model_e_stage1 <- lm(x ~ RANK + POS, data = capm5)
summary(model_e_stage1)
# 檢定聯合顯著性 (F-test)
linearHypothesis(model_e_stage1, c("RANK=0", "POS=0"))

# F
v_hat_joint <- residuals(model_e_stage1)
model_f_test <- lm(y ~ x + v_hat_joint, data = capm5)
summary(model_f_test)

# G
model_g_iv <- ivreg(y ~ x | RANK + POS, data = capm5)
summary(model_g_iv)

# H
e_iv <- residuals(model_g_iv)

# 2. 進行 Sargan 輔助回歸：將 IV 殘差對「所有」工具變數進行回歸
# 注意：此處必須包含截距項與所有工具變數
model_sargan_aux <- lm(e_iv ~ RANK + POS, data = capm5)

# 3. 取得輔助回歸的 R-squared
r2_sargan <- summary(model_sargan_aux)$r.squared

# 4. 計算 Sargan 檢定統計量 (LM = n * R^2)
n <- nrow(capm5)
Sargan_stat <- n * r2_sargan

# 5. 計算 P-value
# 自由度 df = 工具變數數量 - 內生變數數量 = 2 - 1 = 1
p_val_sargan <- 1 - pchisq(Sargan_stat, df = 1)

# 輸出結果
cat("Sargan 統計量 (LM):", Sargan_stat, "\n")
cat("P-value:", p_val_sargan, "\n")
