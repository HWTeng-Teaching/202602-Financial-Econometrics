library(plm)
library(lmtest)
library(sandwich)
url <- "http://www.principlesofeconometrics.com/poe5/data/csv/nls_panel.csv"
data_all <- read.csv(url)
#篩選 1987 和 1988 年的資料
data_sub <- subset(data_all, year %in% c(87, 88))
pdata <- pdata.frame(data_sub, index = c("id", "year"))
# 欄位 (3) 固定效應模型 (Fixed Effects / Within Model)
fe_model <- plm(lwage ~ exper + exper2 + south + union, data = pdata, model = "within")
# 欄位 (5) 隨機效應模型 (Random Effects Model)
re_model <- plm(lwage ~ exper + exper2 + south + union, data = pdata, model = "random")
hausman_result <- phtest(fe_model, re_model)
print(hausman_result)