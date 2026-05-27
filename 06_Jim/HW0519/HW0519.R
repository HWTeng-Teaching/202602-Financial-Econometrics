rm(list=ls())
library(POE5Rdata)
library(stargazer)
# stargazer(results, summary=FALSE, type="latex", 
#           title="Simulation Results", 
#           header=FALSE)
library(ggplot2)
library(gridExtra)
library(AER)
library(car)
library(plm)      # 處理 Panel / Grouped Data 的核心套件
library(lmtest)   # 用於 robust 標準誤與檢定

# 載入 STAR 資料集
data("star")
# 建立公式 (變數全面改為小寫)
fml <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch

# ==============================================================================
# a. Estimate a regression equation (with no fixed or random effects) - Pooled OLS
# ==============================================================================
model_pooled <- lm(fml, data = star)
stargazer(model_pooled, summary=FALSE, type="latex",
          title="(a)",
          header=FALSE)

# ==============================================================================
# b. Reestimate the model with school fixed effects (Within Estimator)
# ==============================================================================
model_fe <- plm(fml, data = star, index = "schid", model = "within")
stargazer(model_fe, summary=FALSE, type="latex",
          title="(b)",
          header=FALSE)
# ==============================================================================
# c. Test for the significance of the school fixed effects
# ==============================================================================
# 比較 FE 模型與 Pooled OLS 模型的 F 檢定
fe_test <- pFtest(model_fe, model_pooled)
fe_test
# ==============================================================================
# d. Reestimate the model with school random effects & LM Test
# ==============================================================================
model_re <- plm(fml, data = star, index = "schid", model = "random")

# Breusch-Pagan LM 檢定
lm_test <- plmtest(model_re, effect = "individual", type = "bp")
lm_test
# ==============================================================================

# e. 依據課本公式 (15.36) 實作「個別變數」的 Hausman t-test
# ==============================================================================

# 1. 提取 FE 與 RE 的係數向量
b_fe <- coef(model_fe)
b_re <- coef(model_re)

# 2. 提取兩個模型共同變數的標準誤 (se)
# (註：因為 RE 有截距項而 FE 沒有，我們用 names(b_fe) 來對齊共同變數)
common_vars <- names(b_fe)

se_fe <- sqrt(diag(vcov(model_fe)))[common_vars]
se_re <- sqrt(diag(vcov(model_re)))[common_vars]

# 3. 嚴格套用公式 (15.36) 的分子與分母
numerator <- b_fe[common_vars] - b_re[common_vars]
denominator <- sqrt(se_fe^2 - se_re^2)

# 4. 計算各自的 t 統計量
t_statistics <- numerator / denominator

# 5. 計算雙尾 p-value (漸進常態分佈)
p_values <- 2 * (1 - pnorm(abs(t_statistics)))

# 6. 整合並列印出與課本完全對應的結果表格
individual_hausman <- data.frame(
  Variable = common_vars,
  b_FE = round(b_fe[common_vars], 4),
  b_RE = round(b_re[common_vars], 4),
  se_FE = round(se_fe, 4),
  se_RE = round(se_re, 4),
  t_stat = round(t_statistics, 4),
  p_val = round(p_values, 5)
)

print(individual_hausman)


# ==============================================================================
# f. Mundlak Test
# ==============================================================================
star$mean_small       <- ave(star$small, star$schid, FUN = mean)
star$mean_aide        <- ave(star$aide, star$schid, FUN = mean)
star$mean_tchexper    <- ave(star$tchexper, star$schid, FUN = mean)
star$mean_boy         <- ave(star$boy, star$schid, FUN = mean)
star$mean_white_asian <- ave(star$white_asian, star$schid, FUN = mean)
star$mean_freelunch   <- ave(star$freelunch, star$schid, FUN = mean)

# 設定 Mundlak 模型公式
model_lm_mundlak <- lm(fml_mundlak, data = star)

fml_mundlak <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch + 
  mean_small + mean_aide + mean_tchexper + mean_boy + mean_white_asian + mean_freelunch

model_lm_mundlak <- lm(fml_mundlak, data = star)
# 3. 進行聯合顯著性檢定 (Wald Test)，並使用學校層級 (schid) 的群聚穩健變異數矩陣
mundlak_joint_test <- linearHypothesis(model_lm_mundlak, 
                                       c("mean_small = 0", 
                                         "mean_aide = 0", 
                                         "mean_tchexper = 0", 
                                         "mean_boy = 0", 
                                         "mean_white_asian = 0", 
                                         "mean_freelunch = 0"),
                                       vcov = vcovCL(model_lm_mundlak, cluster = ~schid))

# 4. 查看檢定結果
print(mundlak_joint_test)

# model_mundlak <- plm(fml_mundlak, data = star, index = "schid", model = "random")

# 對所有平均值項進行聯合顯著性檢定 (Wald Test)

