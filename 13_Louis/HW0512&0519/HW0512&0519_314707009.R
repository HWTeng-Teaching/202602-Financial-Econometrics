rm(list=ls()) #Removes all items in Environment!
library(plm) 
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(POE5Rdata)
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 
library(systemfit)
library(AER)
library(xtable)

data("star", package="POE5Rdata")
# 依據教材規範，建立面板資料結構 (School Fixed/Random Effects)
nlspd_star <- pdata.frame(star, index = c("schid", "id"))

# 檢查面板維度資料
pdim(nlspd_star)

#a
wage.pooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                   model = "pooling", data = nlspd_star)

kable(tidy(wage.pooled), digits = 3, 
      caption = "Pooled OLS model for STAR Experiment")

#b
# b. 使用 within 模型估計學校固定效果
wage.within <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                   data = nlspd_star, model = "within")

kable(tidy(wage.within), digits = 5, 
      caption = "School Fixed Effects using 'within' model")

#c
# c. 固定效果顯著性檢定
kable(tidy(pFtest(wage.within, wage.pooled)), 
      caption = "Fixed effects test: Ho:'No school fixed effects'")

#d
# d. 執行隨機效果 LM 檢定
wageReTest <- plmtest(wage.pooled, effect = "individual")
kable(tidy(wageReTest), caption = "A random effects test for the STAR school equation")

# 依據教材規範，使用 Swamy-Arora (swar) 方法估計隨機效果模型
wage.random <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                   data = nlspd_star, random.method = "swar", model = "random")

kable(tidy(wage.random), digits = 4, 
      caption = "The random effects results for the STAR equation")

#e
# 1. 確保前面步驟的 FE (within) 與 RE (random) 模型已估計完成

# 2. 提取兩模型的係數矩陣
sum_fe <- summary(wage.within)$coefficients
sum_re <- summary(wage.random)$coefficients

# 3. 指定題目要求檢定的變數清單（包含最後探討的 BOY）
vars_to_test <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

# 4. 建立一個空的資料框來儲存計算結果
hausman_t_table <- data.frame(
  Variable = vars_to_test,
  b_FE = NA,
  b_RE = NA,
  se_FE = NA,
  se_RE = NA,
  t_stat = NA,
  p_value = NA
)

# 5. 透過迴圈依序帶入公式 (15.36) 進行計算
for(i in 1:length(vars_to_test)) {
  v <- vars_to_test[i]
  
  b_fe  <- sum_fe[v, "Estimate"]
  se_fe <- sum_fe[v, "Std. Error"]
  b_re  <- sum_re[v, "Estimate"]
  se_re <- sum_re[v, "Std. Error"]
  
  # 實作公式分母：開根號( FE變異數 - RE變異數 )
  denom <- sqrt(se_fe^2 - se_re^2)
  
  # 計算 t 統計量
  t_val <- (b_fe - b_re) / denom
  # 計算標準常態分配下的雙尾 p-value
  p_val <- 2 * (1 - pnorm(abs(t_val)))
  
  # 填入表格
  hausman_t_table$b_FE[i]    <- b_fe
  hausman_t_table$b_RE[i]    <- b_re
  hausman_t_table$se_FE[i]   <- se_fe
  hausman_t_table$se_RE[i]   <- se_re
  hausman_t_table$t_stat[i]  <- t_val
  hausman_t_table$p_value[i] <- p_val
}

# 6. 依據教材規範漂亮輸出表格
kable(hausman_t_table, digits = 4, align = "c",
      caption = "Individual Hausman t-tests (Equation 15.36) for STAR Experiment")


#f
library(dplyr)
library(plm)
library(broom)
library(knitr)
library(car)

# 1. 依據投影片 82/85 頁邏輯，計算各變數的學校時間平均值 (School Averages)
star_mundlak <- star %>%
  group_by(schid) %>%
  mutate(
    bar_small       = mean(small, na.rm = TRUE),
    bar_aide        = mean(aide, na.rm = TRUE),
    bar_tchexper    = mean(tchexper, na.rm = TRUE),
    bar_boy         = mean(boy, na.rm = TRUE),
    bar_white_asian = mean(white_asian, na.rm = TRUE),
    bar_freelunch   = mean(freelunch, na.rm = TRUE)
  ) %>%
  ungroup()

# 轉換為標準 pdata.frame，並修正變數名稱以避免 id 警告
star_mundlak <- star_mundlak %>% rename(student_id = id)
nlspd_md <- pdata.frame(star_mundlak, index = c("schid", "student_id"))

# 1. 估計 Mundlak 混合模型（將 model 改為 "pooling"）
model.mundlak <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                       bar_small + bar_aide + bar_tchexper + bar_boy + bar_white_asian + bar_freelunch,
                     data = nlspd_md, model = "pooling")

# 漂亮輸出參數表
knitr::kable(broom::tidy(model.mundlak), digits = 4, 
             caption = "Mundlak Approach via Pooled OLS (Table 15.6 Style)")

# 2. 執行 Wald 聯合檢定（依據投影片第 82 頁，一定要加入 vcov 進行群集穩健檢定！）
mundlak_wald <- linearHypothesis(model.mundlak, 
                                 c("bar_small = 0", "bar_aide = 0", "bar_tchexper = 0", 
                                   "bar_boy = 0", "bar_white_asian = 0", "bar_freelunch = 0"),
                                 vcov = vcovHC(model.mundlak, type = "HC0", cluster = "group"))

# 輸出最終檢定結果
knitr::kable(broom::tidy(mundlak_wald), caption = "Mundlak Robust Wald Test Results (df = 6)")
