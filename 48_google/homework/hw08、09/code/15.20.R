# ============================================================
#   課程：Financial Econometrics
#   作業：Chapter 15 - Q20
#   姓名：Jun-Gu Chen
# ============================================================

rm(list = ls())

library(POE5Rdata)
library(plm)       # panel data models
library(lmtest)    # coeftest
library(car)       # linearHypothesis

data("star")
head(star)
summary(star)

# 設定面板資料結構（SCHID = 學校，ID = 學生）
pdata <- pdata.frame(star, index = c("schid", "id"))

# -------------------------------------------------------
# (a) OLS（無固定或隨機效果）
# -------------------------------------------------------
ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star)
cat("\n===== (a) OLS 估計結果 =====\n")
print(summary(ols_model))

# -------------------------------------------------------
# (b) 學校固定效果（FE）
# -------------------------------------------------------
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata,
                model = "within",
                effect = "individual")
cat("\n===== (b) 學校固定效果（FE）估計結果 =====\n")
print(summary(fe_model))

# -------------------------------------------------------
# (c) F 檢定固定效果顯著性
# -------------------------------------------------------
cat("\n===== (c) F 檢定：學校固定效果顯著性 =====\n")
fe_test <- pFtest(fe_model, ols_model)
print(fe_test)

# -------------------------------------------------------
# (d) 學校隨機效果（RE）+ LM 檢定
# -------------------------------------------------------
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata,
                model = "random",
                effect = "individual")
cat("\n===== (d) 學校隨機效果（RE）估計結果 =====\n")
print(summary(re_model))

# LM 檢定（Breusch-Pagan）：需使用 plm pooling 物件
pool_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                  data = pdata, model = "pooling", effect = "individual")
cat("\n--- LM 檢定：隨機效果存在性 ---\n")
lm_test <- plmtest(pool_model, type = "bp", effect = "individual")
print(lm_test)

# -------------------------------------------------------
# (e) FE vs RE t 檢定（式 15.36）
# 對每個係數：t = (b_FE - b_RE) / sqrt(Var(b_FE) - Var(b_RE))
# -------------------------------------------------------
cat("\n===== (e) FE vs RE 個別 t 檢定 =====\n")

vars_test <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

coef_fe  <- coef(fe_model)
coef_re  <- coef(re_model)

for (v in vars_test) {
  b_fe     <- coef_fe[v]
  b_re     <- coef_re[v]
  var_diff <- vcov(fe_model)[v, v] - vcov(re_model)[v, v]
  if (var_diff > 0) {
    t_stat <- (b_fe - b_re) / sqrt(var_diff)
    cat(sprintf("%-15s: b_FE = %7.4f, b_RE = %7.4f, Var_diff = %10.6f, t = %7.4f\n",
                v, b_fe, b_re, var_diff, t_stat))
  } else {
    cat(sprintf("%-15s: b_FE = %7.4f, b_RE = %7.4f, Var_diff = %10.6f (非正，無法計算 t)\n",
                v, b_fe, b_re, var_diff))
  }
}

# 整體 Hausman 檢定
cat("\n--- Hausman 檢定（整體）---\n")
hausman <- phtest(fe_model, re_model)
print(hausman)

# -------------------------------------------------------
# (f) Mundlak 檢定
# -------------------------------------------------------
cat("\n===== (f) Mundlak 檢定 =====\n")

# 計算學校層級平均值
school_means <- aggregate(
  cbind(small, aide, tchexper, boy, white_asian, freelunch) ~ schid,
  data = star, FUN = mean
)
colnames(school_means)[-1] <- paste0("m_", colnames(school_means)[-1])

# 合併回原始資料
star_mundlak <- merge(star, school_means, by = "schid")
pdata_m <- pdata.frame(star_mundlak, index = c("schid", "id"))

# 加入學校平均值的隨機效果模型
mundlak_model <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
    m_small + m_aide + m_tchexper + m_boy + m_white_asian + m_freelunch,
  data   = pdata_m,
  model  = "random",
  effect = "individual"
)
print(summary(mundlak_model))

# 聯合顯著性檢定：學校平均值是否聯合顯著
cat("\n--- Mundlak 聯合檢定（學校平均值是否聯合顯著）---\n")
mundlak_test <- linearHypothesis(mundlak_model,
                                 c("m_small = 0", "m_aide = 0",
                                   "m_tchexper = 0", "m_boy = 0",
                                   "m_white_asian = 0", "m_freelunch = 0"))
print(mundlak_test)
