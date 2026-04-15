library(lmtest)
library(sandwich)

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
temp_file <- tempfile(fileext = ".rdata")
download.file(url, destfile = temp_file, mode = "wb")
load(temp_file)

#  建立 OLS 迴歸模型
model_a <- lm(miles ~ income + age + kids, data = vacation)

#  查看模型摘要 (確認估計結果)
summary(model_a)

#  建構 KIDS 變數的 95% 信賴區間
ci_kids <- confint(model_a, "kids", level = 0.95)

ci_lower <- ci_kids[1]  
ci_upper <- ci_kids[2]  

#  最終結果
cat("95% CI: [", ci_lower, ",", ci_upper, "]\n")
