library(POE5Rdata)
library(AER)
library(car)
data(capm5)
?capm5

capm5$excess_msft <- capm5$msft - capm5$riskfree   # r_j - r_f
capm5$excess_mkt  <- capm5$mkt  - capm5$riskfree   # r_m - r_f

# ── Part a：OLS 估計 CAPM ────────────────────────────────────────────────
ols_a <- lm(excess_msft ~ excess_mkt, data = capm5)
summary(ols_a)

# ── Part b：建立 RANK 工具變數 ───────────────────────────────────────────
capm5$RANK <- rank(capm5$excess_mkt)   # 從最小到最大排序，賦值 1~180

first_b <- lm(excess_mkt ~ RANK, data = capm5)
summary(first_b)

# ── Part c：Hausman 檢定（用 RANK 的第一階段殘差） ───────────────────────
capm5$vhat_c <- residuals(first_b)

# 將殘差加入原始方程式，OLS 估計擴充方程式
hausman_c <- lm(excess_msft ~ excess_mkt + vhat_c, data = capm5)
summary(hausman_c)

# ── Part d：IV/2SLS，工具變數 = RANK ─────────────────────────────────────
iv_d <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm5)
summary(iv_d)

# ── Part e：建立 POS，第一階段加入 RANK + POS ────────────────────────────
capm5$POS <- as.integer(capm5$excess_mkt > 0)   # 市場超額報酬為正 → 1

first_e <- lm(excess_mkt ~ RANK + POS, data = capm5)
summary(first_e)
# 聯合顯著性 F-test
linearHypothesis(first_e, c("RANK = 0", "POS = 0"))
# R² 是否比 part(b) 更高？F 是否 > 10？

# ── Part f：Hausman 檢定（用 part e 的第一階段殘差） ────────────────────
capm5$vhat_e <- residuals(first_e)

hausman_f <- lm(excess_msft ~ excess_mkt + vhat_e, data = capm5)
summary(hausman_f)
# vhat_e 在 1% 顯著 → 拒絕外生性

# ── Part g：IV/2SLS，工具變數 = RANK + POS ───────────────────────────────
iv_g <- ivreg(excess_msft ~ excess_mkt | RANK + POS, data = capm5)
summary(iv_g)

# ── Part h：手動 Sargan 檢定（5% 顯著水準） ─────────────────────────────
# 超額識別：2 個工具 - 1 個內生變數 = 1 個自由度
capm5$ehat_g <- residuals(iv_g)

sargan_reg <- lm(ehat_g ~ RANK + POS, data = capm5)
n  <- nrow(capm5)
R2 <- summary(sargan_reg)$r.squared
sargan_stat <- n * R2
sargan_stat
p_value <- 1 - pchisq(sargan_stat, df = 1)   # df = 2 - 1 = 1
p_value
# p > 0.05 → 無法拒絕工具有效性，surplus IV（POS）通過 Sargan 檢定