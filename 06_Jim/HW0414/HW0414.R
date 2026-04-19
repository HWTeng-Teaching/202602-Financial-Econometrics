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
# library(modelsummary)
# modelsummary(model_loglin, output = "latex")

#6a
97161.9174/(577-4)
12.024**2
(97161.9174/(577-4))/(12.024**2)
qf(0.05/2,577-4,1000-577-4)
qf(1-0.05/2,577-4,1000-577-4)

#6b
{56231.0382}/{400 - 5}
{100703.0471}/{600 - 5} 
({100703.0471}/{600 - 5})/({56231.0382}/{400 - 5})
qf(1-0.05,600-5,400-5)

#6c
qchisq(1-0.05,4)
#6d
qchisq(1-0.05,12)

#16a
data(vacation)
model_ols <- lm(miles ~ income + age + kids, data = vacation)
summary(model_ols)
stargazer(model_ols, summary=FALSE, type="latex", 
          header=FALSE)
conf_a <- confint(model_ols, "kids", level = 0.95)
cat("(a) OLS 下 KIDS 的 95% 信賴區間:", conf_a, "\n")


# --- (b) 殘差圖診斷 ---
# 繪製殘差對 INCOME 與 AGE 的圖
par(mfrow=c(1,2))
plot(vacation$income, resid(model_ols), main="Residuals vs INCOME", xlab="income", ylab="Residuals")
abline(h=0, col="red")
plot(vacation$age, resid(model_ols), main="Residuals vs AGE", xlab="age", ylab="Residuals")
abline(h=0, col="red")
# 觀察：若殘差隨 INCOME 增加而呈「漏斗狀」擴散，則存在異質變異數。


# --- (c) Goldfeld-Quandt 檢定 ---
# 依照 INCOME 排序
vacation_sorted <- vacation[order(vacation$income), ]

# H0: 變異數同質 (sigma^2_low = sigma^2_high)
# H1: 變異數隨 INCOME 增加而增加 (sigma^2_low < sigma^2_high)
# n1=90, n2=90, 中間省略 20 筆
gq_test <- gqtest(model_ols, fraction = 20, order.by = ~ income, data = vacation)
print(gq_test)
gq_table <- matrix(c(
  gq_test$statistic,
  gq_test$parameter[1],
  gq_test$parameter[2],
  gq_test$p.value
), ncol = 1)

rownames(gq_table) <- c("GQ Statistic", "df1 (numerator)", "df2 (denominator)", "p-value")
colnames(gq_table) <- "Goldfeld-Quandt Test"

# 3. 輸出 LaTeX 代碼
stargazer(gq_table, 
          type = "latex", 
          title = "Goldfeld-Quandt Test for Heteroskedasticity",
          decimal.mark = ".",
          digits = 4, # 控制小數點位數
          header = FALSE)


# --- (d) White 穩健標準誤差 (Heteroskedasticity Robust SE) ---
# 使用 HC1 (通常對應教材中的 White SE)
robust_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
print(robust_se)

# 計算 Robust 信賴區間
# 區間公式：beta +/- critical_value * robust_se
se_kids_robust <- sqrt(vcovHC(model_ols, type = "HC1")["kids", "kids"])
beta_kids <- coef(model_ols)["kids"]
df_deg <- df.residual(model_ols)
conf_d <- c(beta_kids - qt(0.975, df_deg) * se_kids_robust, 
            beta_kids + qt(0.975, df_deg) * se_kids_robust)
cat("(d) Robust SE 下 KIDS 的 95% 信賴區間:", conf_d, "\n")


# --- (e) GLS 估計 (假設變異數正比於 INCOME^2) ---
# 權重設定為 1 / INCOME^2
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = 1/(income^2))
summary(model_gls)

# GLS 傳統信賴區間
conf_e_trad <- confint(model_gls, "kids", level = 0.95)

# GLS 穩健信賴區間 (Robust GLS)
se_kids_gls_robust <- sqrt(vcovHC(model_gls, type = "HC1")["kids", "kids"])
beta_kids_gls <- coef(model_gls)["kids"]
conf_e_robust <- c(beta_kids_gls - qt(0.975, df_deg) * se_kids_gls_robust, 
                   beta_kids_gls + qt(0.975, df_deg) * se_kids_gls_robust)

cat("(e) GLS 傳統區間:", conf_e_trad, "\n")
cat("(e) GLS Robust 區間:", conf_e_robust, "\n")
