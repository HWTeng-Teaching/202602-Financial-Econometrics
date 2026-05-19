# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 11 - Q24
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)
library(AER)     # ivreg (2SLS)
library(car)     # linearHypothesis

data("fultonfish")
head(fultonfish)
summary(fultonfish)

# -------------------------------------------------------
# (a) 縮減型方程式 for ln(PRICE)，加入 MIXED
# -------------------------------------------------------
reduced_price <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed,
                    data = fultonfish)
cat("\n===== (a) 縮減型方程式 ln(PRICE) =====\n")
summary(reduced_price)

# MIXED 個別顯著性
cat("\n--- MIXED 係數與 t 值 ---\n")
print(coef(summary(reduced_price))["mixed", ])

# STORMY & MIXED 聯合 F 檢定
cat("\n--- STORMY & MIXED 聯合 F 檢定 ---\n")
f_stormy_mixed <- linearHypothesis(reduced_price,
                                   c("stormy = 0", "mixed = 0"))
print(f_stormy_mixed)

# -------------------------------------------------------
# (b) 2SLS 需求方程式，IV = STORMY + MIXED
# -------------------------------------------------------
demand_2sls <- ivreg(lquan ~ lprice + mon + tue + wed + thu |
                       mon + tue + wed + thu + stormy + mixed,
                     data = fultonfish)
cat("\n===== (b) 2SLS 需求方程式（IV: STORMY, MIXED）=====\n")
print(summary(demand_2sls, diagnostics = TRUE))

# -------------------------------------------------------
# (c) Sargan 過度識別檢定
# -------------------------------------------------------
resid_2sls  <- residuals(demand_2sls)
sargan_aux  <- lm(resid_2sls ~ mon + tue + wed + thu + stormy + mixed,
                  data = fultonfish)
n           <- nrow(fultonfish)
r2_sargan   <- summary(sargan_aux)$r.squared
sargan_stat <- n * r2_sargan
p_sargan    <- 1 - pchisq(sargan_stat, df = 1)  # df = #IV - #內生變數 = 2 - 1 = 1

cat("\n===== (c) Sargan 過度識別檢定 =====\n")
cat(sprintf("Sargan 統計量 = %.4f\n", sargan_stat))
cat(sprintf("自由度 = 1\n"))
cat(sprintf("p 值 = %.4f\n", p_sargan))

# -------------------------------------------------------
# (d) MON, TUE, WED, THU 聯合 F 檢定（在縮減型方程式中）
# -------------------------------------------------------
cat("\n===== (d) MON, TUE, WED, THU 聯合 F 檢定 =====\n")
f_days <- linearHypothesis(reduced_price,
                           c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
print(f_days)
