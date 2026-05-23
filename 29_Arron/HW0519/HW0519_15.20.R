#new_packages 是空的話（長度為 0），if 判定為 FALSE，就不會下載
#就算長度是1、2，都會執行下載
#下載時只會載True
#installed.packages()[, "Package"]這邊將有安裝的package拉出來，沒有下載的顯示為False
#但因為驚嘆號，把它反轉為True
#[...] 裡面裝的則是「篩選條件」
#檢查「左邊向量的每一個元素」，有沒有出現在「右邊的集合（或向量）」裡面

required_packages <- c("plm", "lmtest")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(plm)
library(lmtest)

# 從官網直接下載並載入 star.rdata
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"
con <- url(url)
load(con)
close(con)
head(star)

# -------------------------------------------------------------------------
# a. OLS 迴歸模型估計結果
cat("\n=== (a) OLS 迴歸模型估計結果 ===\n")
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
print(summary(model_a))


# -------------------------------------------------------------------------
p_star <- pdata.frame(star, 
                      index = c("schid", "id"),  
                      drop.index = FALSE)


cat("=== Panel 資料結構檢查 ===\n")
print(head(attr(p_star, "index")))

# -------------------------------------------------------------------------
# b 小題：學校固定效果模型 (School Fixed Effects)
# -------------------------------------------------------------------------
cat("\n=== (b) 學校固定效果模型 (School Fixed Effects) 估計結果 ===\n")

model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
               data = p_star, 
               model = "within", 
               effect = "individual")

print(summary(model_b))
# -------------------------------------------------------------------------
# c. 學校固定效果顯著性檢定 (F-test)
#pFtest(固定效果模型 FE, 混合 OLS 模型 Pooled OLS)
#加入學校固定效果（model_b）後，模型的誤差（RSS）大幅度下降，代表控制學校差異非常有用
#pFtest 計算下降幅度在統計上是否顯著
# -------------------------------------------------------------------------
cat("\n=== (c) 學校固定效果之顯著性檢定 (F-test) ===\n")
joint_test <- pFtest(model_b, model_a)
print(joint_test)

# -------------------------------------------------------------------------
# d 小題：學校隨機效果模型 (School Random Effects) 與 LM 檢定
# -------------------------------------------------------------------------
cat("\n=== (d) 學校隨機效果模型 (School Random Effects) 估計結果 ===\n")

# 隨機效果模型，預的 swar 估計法
model_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
               data = p_star, 
               model = "random", 
               random.method = "swar",
               effect = "individual")

print(summary(model_d))

cat("\n=== (d) 隨機效果存在性檢定 (Breusch-Pagan LM Test) ===\n")
# plmtest(..., type="bp") 檢定 Pooled OLS vs Random Effects
# 虛無假設 H0: 學校個體效果變異數為 0 (不需使用隨機效果)
lm_test <- plmtest(model_d, type = "bp")
print(lm_test)


# -------------------------------------------------------------------------
# e 小題：依據公式 (15.36) 執行 t 檢定
# -------------------------------------------------------------------------
cat("\n=== (e) (15.36) 各變數之 Hausman t 檢定 ===\n")

# 1. 提取 FE (model_b) 與 RE (model_d) 的係數向量 (RE 需扣除截距項)
b_fe <- coef(model_b)
b_re <- coef(model_d)[-1] 

# 2. 提取兩模型的對角線標準誤平方 (變異數)
se_var_fe <- diag(vcov(model_b))
se_var_re <- diag(vcov(model_d))[-1] 

# 3. 
denominator <- sqrt(se_var_fe - se_var_re)
hausman_t   <- (b_fe - b_re) / denominator

# 4. 算出 Z 分配雙尾檢定 p 值(一律用Z表)
hausman_p   <- 2 * (1 - pnorm(abs(hausman_t)))

# 5. 
hausman_t_summary <- data.frame(
  Beta_FE = b_fe,
  Beta_RE = b_re,
  t_stat  = hausman_t,
  p_value = hausman_p
)
print(round(hausman_t_summary, 5))

cat("\n=== (e) 補充： phtest 聯合卡方檢定結果 (對照用) ===\n")
# 全體變數的卡方檢定
hausman_joint <- phtest(model_b, model_d)
print(hausman_joint)


#f-------------------------------------------------
if(!require(lmtest)) install.packages("lmtest")
if(!require(sandwich)) install.packages("sandwich")
if(!require(car)) install.packages("car")
library(lmtest)
library(sandwich)
library(car)

cat("\n=== (f) Regression-Based Hausman Test ===\n")

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
