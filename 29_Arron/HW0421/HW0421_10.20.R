# 如果還沒安裝過 devtools，請先取消下面這行的註解執行一次
# install.packages("devtools")

# 從 GitHub 安裝 PoE 第五版數據包
# 有時候不同的箱子有同名的工具，這樣寫能精確指定。
devtools::install_github("ccolonescu/POE5Rdata")

library(POE5Rdata)
library(AER)     # 用於 IV 估計
library(dplyr)   # 用於資料處理

# 載入 capm5 數據
data(capm5)
df <- capm5

# --- 準備變數 ---
# 題目要求：r_j - r_f = alpha + beta * (r_m - r_f)
# capm5 變數名：msft (微軟), mkt (市場), riskfree (無風險利率)
df <- df %>%
  mutate(y = msft - riskfree,
         x = mkt - riskfree)
#df %>%：是「然後」的意思
#mutate(...)：「新增欄位」的指令

# (a) OLS 估計
ols_model <- lm(y ~ x, data = df)
summary(ols_model)

# (b) 建立 RANK 工具變數並進行第一階段
df <- df %>% mutate(RANK = rank(x))
first_stage_b <- lm(x ~ RANK, data = df)
summary(first_stage_b)

# (c) 殘差法進行內生性檢定 (Hausman Test 手動法)
v_hat <- residuals(first_stage_b)
augmented_reg <- lm(y ~ x + v_hat, data = df)
summary(augmented_reg)

# (d) 2SLS 估計 (使用 RANK)
iv_model_d <- ivreg(y ~ x | RANK, data = df)
summary(iv_model_d)

# (e) 增加 POS 工具變數
df <- df %>% mutate(POS = ifelse(x > 0, 1, 0))
first_stage_e <- lm(x ~ RANK + POS, data = df)
summary(first_stage_e)

# (f) & (g) 使用兩個 IV 進行 2SLS 與內建檢定
iv_model_g <- ivreg(y ~ x | RANK + POS, data = df)
summary(iv_model_g, diagnostics = TRUE) # 這裡會包含 Wu-Hausman 與 Sargan 檢定

# (h) 手動 Sargan 檢定 (Overidentifying Restrictions)
iv_res <- residuals(iv_model_g)
sargan_reg <- lm(iv_res ~ RANK + POS, data = df)
sargan_stat <- nrow(df) * summary(sargan_reg)$r.squared
p_val_sargan <- 1 - pchisq(sargan_stat, df = 1) # 2個IV - 1個內生變數 = df 1

cat("Sargan 檢定統計量:", sargan_stat, "\nP值:", p_val_sargan)