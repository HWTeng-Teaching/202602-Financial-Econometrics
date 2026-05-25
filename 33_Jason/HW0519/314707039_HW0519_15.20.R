library(plm)
library(lmtest)
library(dplyr)
library(car)
library(sandwich)
url <- "http://www.principlesofeconometrics.com/poe5/data/csv/star.csv"
star_data <- read.csv(url)
star <- read.csv(url)
#a小題
model_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = star_data)
print(summary(model_ols))

#b小題
p_star <- pdata.frame(star_data, index = c("schid", "id"))
model_fe <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = p_star, model = "within")
print(summary(model_fe))

#c小題
#比較 FE 模型與 Pooled OLS 模型是否有系統性差異
print(pFtest(model_fe, model_ols))

#d小題
model_re <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = p_star, model = "random")
cat("【題 d】學校隨機效應模型 (Random Effects) 估計結果\n")
print(summary(model_re))
cat("Breusch-Pagan LM 檢定結果 (H0: 無隨機效應)\n")
print(plmtest(model_re, type = "bp"))

#e小題
b_fe <- coef(model_fe)
b_re <- coef(model_re)[names(b_fe)] # 確保變數順序一致並排除 RE 的截距項
se_fe <- sqrt(diag(vcov(model_fe)))
se_re <- sqrt(diag(vcov(model_re)))[names(b_fe)]
variables <- names(b_fe)
results <- data.frame(
  Variable = variables,
  b_FE = round(b_fe, 5),
  b_RE = round(b_re, 5),
  se_FE = round(se_fe, 5),
  se_RE = round(se_re, 5),
  t_stat = NA,
  p_value = NA,
  stringsAsFactors = FALSE
)

# 依據公式 (15.36) 逐個變數進行迴圈計算
for (i in 1:nrow(results)) {
  # 分子：係數差
  num <- b_fe[i] - b_re[i]
  
  # 分母根號內：標準誤的平方差 (變異數差)
  var_diff <- (se_fe[i]^2) - (se_re[i]^2)
  
  if (var_diff > 0) {
    # 正常情況：分母為正數，可順利開根號
    t_stat <- num / sqrt(var_diff)
    p_val <- 2 * (1 - pnorm(abs(t_stat))) # 雙尾 t 檢定 p-value
    
    results$t_stat[i] <- round(t_stat, 4)
    results$p_value[i] <- round(p_val, 4)
  } }
print(results, row.names = FALSE)


#f小題
# 1
star$mean_small     <- ave(star$small,     star$schid, FUN = mean)
star$mean_aide      <- ave(star$aide,      star$schid, FUN = mean)
star$mean_tchexper  <- ave(star$tchexper,  star$schid, FUN = mean)
star$mean_freelunch <- ave(star$freelunch, star$schid, FUN = mean)

# 2
mundlak <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch + 
                mean_small + mean_aide + mean_tchexper + mean_freelunch, 
              data = star)

robust_vcov <- vcovCL(mundlak, cluster = ~schid)

# 3.
cat("\n--- OLS 迴歸估計結果（穩健標準誤） ---\n")
print(coeftest(mundlak, vcov = robust_vcov))


cat("\n=== (f) 執行 Wald 檢定（Mundlak 檢定） ===\n")
# 4 聯合 Wald 檢定，檢定 4 個平均值項目的斜率是否聯合為 0
mundlak_wald <- linearHypothesis(
  mundlak, 
  c("mean_small = 0", 
    "mean_aide = 0", 
    "mean_tchexper = 0", 
    "mean_freelunch = 0"),
  vcov = robust_vcov
)

print(mundlak_wald)