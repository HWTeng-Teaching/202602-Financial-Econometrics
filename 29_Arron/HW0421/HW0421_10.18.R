# 1. 安裝與載入必要的套件
# install.packages("PoEdata") 
# install.packages("AER") 
#AER：經濟學專用套件，裡面有 ivreg 函數
library(PoEdata)
library(AER)

# 2. 載入資料並篩選參與勞動市場的女性 (lfp == 1)
#Labor Force Participation
data(mroz)
mroz_working <- subset(mroz, lfp == 1)

# --- (a) 建立虛擬變數與計算比例 ---
mroz_working$MOTHERCOLL <- ifelse(mroz_working$mothereduc > 12, 1, 0)
mroz_working$FATHERCOLL <- ifelse(mroz_working$fathereduc > 12, 1, 0)

# 計算父母受過大學教育的百分比
pct_mother <- mean(mroz_working$MOTHERCOLL) * 100
pct_father <- mean(mroz_working$FATHERCOLL) * 100
cat("母親大學教育比例:", pct_mother, "%\n")
cat("父親大學教育比例:", pct_father, "%\n")

# --- (b) 計算相關係數 ---
cor_matrix <- cor(mroz_working[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print("相關係數矩陣:")
print(cor_matrix)

# --- (c) 使用 MOTHERCOLL 作為 IV 估計工資方程式 ---
# 假設工資方程式包含 exper 與 exper^2 
iv_model_c <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                      MOTHERCOLL + exper + I(exper^2), data = mroz_working)

summary(iv_model_c)
confint(iv_model_c, "educ", level = 0.95) # 95% 信賴區間

# --- (d) 第一階段回歸與 F 檢定 ---
first_stage_d <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = mroz_working)
summary(first_stage_d)
# F 檢定：MOTHERCOLL 是否為強工具變數
linearHypothesis(first_stage_d, "MOTHERCOLL = 0")

# --- (e) 使用 MOTHERCOLL 與 FATHERCOLL 作為 IV ---
iv_model_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                      MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_working)

summary(iv_model_e)
confint(iv_model_e, "educ", level = 0.95)

# --- (f) 雙工具變數的第一階段 F 檢定 ---
first_stage_f <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_working)
summary(first_stage_f)
linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

# --- (g) 過度識別檢定 (Sargan Test / Validity of surplus instrument) ---
# 在 ivreg 的 summary 中加上 diagnostics=TRUE 即可看到
summary(iv_model_e, diagnostics = TRUE)SS