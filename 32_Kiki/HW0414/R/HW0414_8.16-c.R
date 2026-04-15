library(lmtest)
library(sandwich)

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
temp_file <- tempfile(fileext = ".rdata")
download.file(url, destfile = temp_file, mode = "wb")
load(temp_file)

# --- c 小題：Goldfeld-Quandt 檢定 ---
# 排序並分組 (前 90 筆 vs 後 90 筆)
vacation_sorted <- vacation[order(vacation$income), ]
data_low <- vacation_sorted[1:90, ]
data_high <- vacation_sorted[111:200, ]

model_low <- lm(miles ~ income + age + kids, data = data_low)
model_high <- lm(miles ~ income + age + kids, data = data_high)

sse_low <- sum(resid(model_low)^2)
sse_high <- sum(resid(model_high)^2)

gq_stat <- (sse_high / 86) / (sse_low / 86)
p_val_gq <- 1 - pf(gq_stat, 86, 86)

cat("--- (c) Goldfeld-Quandt 檢定 ---\n")
cat("GQ 統計量:", gq_stat, "\n")
cat("P-value:", p_val_gq, "\n\n")