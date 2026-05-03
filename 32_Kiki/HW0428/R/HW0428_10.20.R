
library(AER)
library(car)

#  讀取資料 
url_link <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
load(url(url_link))

# 變數定義：手動計算超額報酬 (Excess Returns) 並存回資料集
# 原理：超額報酬 = 資產或市場報酬率 - 無風險利率
capm5$msft_rf <- capm5$msft - capm5$riskfree  # 應變數：微軟超額報酬
capm5$mkt_rf <- capm5$mkt - capm5$riskfree    # 自變數：市場超額報酬


y <- capm5$msft_rf 
x <- capm5$mkt_rf  

# a. 使用 OLS 估計 CAPM 模型
ols_model <- lm(msft_rf ~ mkt_rf, data = capm5)
# (註：這裡寫成 ols_model <- lm(y ~ x) 也可以)
summary(ols_model)

# b. 建立工具變數 RANK (由小到大排序)
# rank() 函數會處理這個邏輯
capm5$RANK <- rank(capm5$x)

# 第一階段迴歸：將內生變數 x 對工具變數 RANK 跑迴歸
first_stage_b <- lm(x ~ RANK, data = capm5)
summary(first_stage_b)


# c. 取得第一階段迴歸的殘差 v_hat
first_stage <- lm(mkt_rf ~ RANK, data = capm5)
capm5$v_hat <- resid(first_stage)

#  把這個算出來的 v_hat 丟進原本的 CAPM 模型裡，跑一個「擴增迴歸」
aug_model_c <- lm(msft_rf ~ mkt_rf + v_hat, data = capm5)

#  報表
summary(aug_model_c)

# d. 使用 RANK 作為 IV 進行 2SLS 估計
# ivreg 的語法是：依變數 ~ 內生變數 | 工具變數
iv_model_d <- ivreg(y ~ x | RANK, data = capm5)
summary(iv_model_d)

# e. 建立虛擬變數 POS：當市場超額報酬為正時 = 1，否則 = 0
capm5$POS <- ifelse(capm5$x > 0, 1, 0)

# 使用 RANK 和 POS 兩個工具變數進行第一階段迴歸
first_stage_e <- lm(x ~ RANK + POS, data = capm5)
summary(first_stage_e)

# 檢定 IV 的聯合顯著性 (Joint significance) : H0: RANK=0 且 POS=0
linearHypothesis(first_stage_e, c("RANK = 0", "POS = 0"))


# f. 取得 (e) 小題第一階段迴歸的殘差
capm5$v_hat_e <- resid(first_stage_e)

# 將新的殘差加入原模型檢測內生性
aug_model_f <- lm(y ~ x + v_hat_e, data = capm5)
summary(aug_model_f)

# g. 使用 RANK 和 POS 作為 IV 進行 2SLS 估計
iv_model_g <- ivreg(y ~ x | RANK + POS, data = capm5)
summary(iv_model_g)

# h. 取得 (g) 小題 2SLS 模型的殘差
capm5$iv_resid <- resid(iv_model_g)

# 將 2SLS 的殘差對「所有外生變數 (包含所有 IV)」跑輔助迴歸
sargan_reg <- lm(iv_resid ~ RANK + POS, data = capm5)
summary_sargan <- summary(sargan_reg)

# 樣本數 N
N <- nrow(capm5)

# 輔助迴歸的 R-squared
R2_sargan <- summary_sargan$r.squared

# 計算 Sargan 檢定統計量 (N * R^2)
sargan_stat <- N * R2_sargan

# 計算自由度：工具變數數量(2) - 內生變數數量(1) = 1
df_sargan <- 2 - 1 

# 計算 p-value (卡方分配)
p_value_sargan <- 1 - pchisq(sargan_stat, df = df_sargan)

# 印出結果
cat("Sargan Statistic:", sargan_stat, "\n")
cat("p-value:", p_value_sargan, "\n")