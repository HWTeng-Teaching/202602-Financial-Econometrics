# =========================
# Exercise 8.16 (Fixed version)
# =========================

# ---- 套件 ----
library(POE5Rdata)
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)

# ---- 載入資料 ----
data("vacation")
df <- vacation

# 檢查變數名稱（重要）
names(df)
str(df)

# =========================
# (a) OLS + 95% CI for kids
# =========================

ols_mod <- lm(miles ~ income + age + kids, data = df)
summary(ols_mod)

# 95% 信賴區間（內建）
confint(ols_mod, "kids", level = 0.95)

# 手動版本
b_kids <- coef(summary(ols_mod))["kids", "Estimate"]
se_kids <- coef(summary(ols_mod))["kids", "Std. Error"]

df_resid_ols <- df.residual(ols_mod)
t_crit <- qt(0.975, df = df_resid_ols)

ci_kids_ols <- c(
  lower = b_kids - t_crit * se_kids,
  upper = b_kids + t_crit * se_kids
)
ci_kids_ols

# =========================
# (b) 殘差圖
# =========================

df$resid_ols <- resid(ols_mod)

# residual vs income
ggplot(df, aes(x = income, y = resid_ols)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs income")

# residual vs age
ggplot(df, aes(x = age, y = resid_ols)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs age")

# =========================
# (c) Goldfeld-Quandt test
# =========================

df_sorted <- df %>% arrange(income)

low90  <- df_sorted[1:90, ]
high90 <- df_sorted[(nrow(df_sorted)-89):nrow(df_sorted), ]

mod_low  <- lm(miles ~ income + age + kids, data = low90)
mod_high <- lm(miles ~ income + age + kids, data = high90)

SSR_low  <- sum(resid(mod_low)^2)
SSR_high <- sum(resid(mod_high)^2)

df_gq <- 90 - 4

F_gq <- (SSR_high / df_gq) / (SSR_low / df_gq)
F_gq

pval_gq <- 1 - pf(F_gq, df1 = df_gq, df2 = df_gq)
pval_gq

crit_gq <- qf(0.95, df1 = df_gq, df2 = df_gq)
crit_gq

# =========================
# (d) Robust SE
# =========================

robust_vcov <- vcovHC(ols_mod, type = "HC1")
coeftest(ols_mod, vcov = robust_vcov)

robust_se_kids <- sqrt(diag(robust_vcov))["kids"]

ci_kids_robust <- c(
  lower = b_kids - t_crit * robust_se_kids,
  upper = b_kids + t_crit * robust_se_kids
)
ci_kids_robust

# =========================
# (e) GLS (WLS)
# var(e_i) = sigma^2 * income^2
# =========================

gls_mod <- lm(miles ~ income + age + kids,
              data = df,
              weights = 1 / (income^2))

summary(gls_mod)

confint(gls_mod, "kids", level = 0.95)

# robust GLS
gls_robust_vcov <- vcovHC(gls_mod, type = "HC1")
coeftest(gls_mod, vcov = gls_robust_vcov)

b_kids_gls <- coef(summary(gls_mod))["kids", "Estimate"]
se_kids_gls <- coef(summary(gls_mod))["kids", "Std. Error"]
robust_se_kids_gls <- sqrt(diag(gls_robust_vcov))["kids"]

df_resid_gls <- df.residual(gls_mod)
t_crit_gls <- qt(0.975, df = df_resid_gls)

ci_kids_gls <- c(
  lower = b_kids_gls - t_crit_gls * se_kids_gls,
  upper = b_kids_gls + t_crit_gls * se_kids_gls
)

ci_kids_gls_robust <- c(
  lower = b_kids_gls - t_crit_gls * robust_se_kids_gls,
  upper = b_kids_gls + t_crit_gls * robust_se_kids_gls
)

# =========================
# 比較結果
# =========================

comparison <- data.frame(
  Method = c("OLS", "OLS robust", "GLS", "GLS robust"),
  Estimate = c(b_kids, b_kids, b_kids_gls, b_kids_gls),
  SE = c(se_kids, robust_se_kids, se_kids_gls, robust_se_kids_gls),
  Lower = c(ci_kids_ols["lower"], ci_kids_robust["lower"],
            ci_kids_gls["lower"], ci_kids_gls_robust["lower"]),
  Upper = c(ci_kids_ols["upper"], ci_kids_robust["upper"],
            ci_kids_gls["upper"], ci_kids_gls_robust["upper"])
)

print(comparison)