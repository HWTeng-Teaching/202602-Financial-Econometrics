library(plm)       # panel data models
library(lmtest)    # coeftest
library(car)   

#a
ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,data = star)
print(summary(ols_model))

#b
pdata <- pdata.frame(star, index = c("schid", "id"))
fixed_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata, model = "within", effect = "individual")
print(summary(fixed_model))

#c
fe_test <- pFtest(fixed_model, ols_model)
print(fe_test)

#d
random_model <-  plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata, model = "random", effect = "individual")
print(summary(random_model))

# 2. 執行 Breusch-Pagan LM 檢定 (檢定隨機效果是否存在)
# LM 檢定通常以 Pooling OLS 模型為基礎來進行測試
pooled_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star, index = c("schid", "id"), model = "pooling", effect = "individual")
plmtest(pooled_model, type = "bp")

#e
b_fe <- coef(fixed_model)
b_re <- coef(random_model)
var_fe <- diag(vcov(fixed_model))
var_re <- diag(vcov(random_model))
test_vars <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

#  建立一個空的資料表來儲存計算結果
results <- data.frame(
  Variable = test_vars,
  b_FE = numeric(length(test_vars)),
  b_RE = numeric(length(test_vars)),
  var_diff = numeric(length(test_vars)),
  t_statistic = numeric(length(test_vars)),
  p_value = numeric(length(test_vars)),
  Significant_5pct = character(length(test_vars))
)
for (i in seq_along(test_vars)) {
  v <- test_vars[i]
  
  # 確保變數存在於模型中
  if(v %in% names(b_fe) && v %in% names(b_re)) {
    
    results$b_FE[i] <- round(b_fe[v], 4)
    results$b_RE[i] <- round(b_re[v], 4)
    results$Var_FE[i] <- signif(var_fe[v], 4)
    results$Var_RE[i] <- signif(var_re[v], 4)
    
    # 計算公式分母內部的變異數差：var(b_FE) - var(b_RE)
    var_diff <- var_fe[v] - var_re[v]
    
    # 理論上漸近大樣本下 var_diff 應大於 0
    # 但在有限樣本下若 var_diff < 0，數學上無法開根號，通常視為兩者無顯著差異
    if (var_diff > 0) {
      # 套用公式 15.36
      t_stat <- (b_fe[v] - b_re[v]) / sqrt(var_diff)
      # 計算雙尾檢定的 p-value
      p_val <- 2 * (1 - pnorm(abs(t_stat))) 
      
      results$t_statistic[i] <- round(t_stat, 4)
      results$p_value[i] <- round(p_val, 4)
      results$Significant_5pct[i] <- ifelse(p_val < 0.05, "Yes", "No")
      
    } else {
      results$t_statistic[i] <- NA
      results$p_value[i] <- NA
      results$Significant_5pct[i] <- "No (Var Diff < 0)"
    }
  }
}

# 顯示最終的檢定結果表
print("--- 根據公式 (15.36) 的個別變數 t 檢定結果 ---")
print(results)

hausman_test_result <- phtest(fixed_model, random_model)
print("--- 標準 Hausman 檢定結果 (整體模型) ---")
print(hausman_test_result)

#f
# 1. 建立各變數的「學校平均值」 (School-averages)
# 使用 group_by 針對每個學校 (schid) 計算平均，並新增為新的欄位
star_mundlak <- star %>%
  group_by(schid) %>%
  mutate(
    mean_small       = mean(small, na.rm = TRUE),
    mean_aide        = mean(aide, na.rm = TRUE),
    mean_tchexper    = mean(tchexper, na.rm = TRUE),
    mean_white_asian = mean(white_asian, na.rm = TRUE),
    mean_freelunch   = mean(freelunch, na.rm = TRUE),
    mean_boy         = mean(boy, na.rm = TRUE)
  ) %>%
  ungroup()

# 2. 估計 Mundlak 隨機效果模型
# 在原有的 RE 模型中，同時放入「原始變數」與剛剛算出來的「學校平均變數」
mundlak_model <- plm(readscore ~ small + aide + tchexper + white_asian + freelunch + boy +
                       mean_small + mean_aide + mean_tchexper + 
                       mean_white_asian + mean_freelunch + mean_boy, 
                     data = star_mundlak, 
                     index = c("schid", "id"), 
                     model = "random",
                     random.method = "walhus")

summary(mundlak_model)

linearHypothesis(mundlak_model, 
                 c("mean_small = 0", 
                   "mean_aide = 0", 
                   "mean_tchexper = 0", 
                   "mean_white_asian = 0", 
                   "mean_freelunch = 0", 
                   "mean_boy = 0"))
