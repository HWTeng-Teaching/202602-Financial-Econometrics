# 載入需要的套件
library(AER)
library(car)

# 資料 
url_link <- "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"
load(url(url_link))

#(a)
# 篩選出參與勞動力的 428 個觀察值
mroz_work <- subset(mroz, lfp == 1)


# 建立 MOTHERCOLL 與 FATHERCOLL (學歷 > 12 為 1，否則為 0)
mroz_work$MOTHERCOLL <- ifelse(mroz_work$mothereduc > 12, 1, 0)
mroz_work$FATHERCOLL <- ifelse(mroz_work$fathereduc > 12, 1, 0)

# 計算比例 (乘以 100 轉為百分比)
pct_mother <- mean(mroz_work$MOTHERCOLL) * 100
pct_father <- mean(mroz_work$FATHERCOLL) * 100
pct_any <- mean(mroz_work$MOTHERCOLL == 1 | mroz_work$FATHERCOLL == 1) * 100
install.packages(c("AER", "car"))

print(paste("母親有大學學歷:", pct_mother))
print(paste("父親有大學學歷:", pct_father))

#(b)
# 計算 EDUC, MOTHERCOLL, FATHERCOLL 之間的相關係數矩陣
cor_matrix <- cor(mroz_work[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print(cor_matrix)

#(c)
# 使用 MOTHERCOLL 作為 EDUC 的工具變數
iv_model_c <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + MOTHERCOLL, data = mroz_work)
summary(iv_model_c)

# 取得 EDUC 的 95% 信賴區間
confint(iv_model_c, "educ", level = 0.95)

#(d)
# 第一階段迴歸：將內生變數 educ 對所有外生變數 (包含 IV) 跑迴歸
first_stage_d <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = mroz_work)

# 進行 F 檢定
linearHypothesis(first_stage_d, "MOTHERCOLL = 0")

#(e)
# 加入 FATHERCOLL 作為第二個工具變數
iv_model_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz_work)
summary(iv_model_e)

# 取得新的 95% 信賴區間
confint(iv_model_e, "educ", level = 0.95)

#(f)
first_stage_f <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz_work)

# 聯合顯著性檢定 (Joint significance)
linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#(g)
# 加上 diagnostics = TRUE 就可以直接印出 Sargan test 結果
summary(iv_model_e, diagnostics = TRUE)
