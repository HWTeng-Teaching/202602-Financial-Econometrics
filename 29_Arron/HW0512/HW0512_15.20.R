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