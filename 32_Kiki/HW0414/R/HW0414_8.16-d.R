library(lmtest)
library(sandwich)

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
temp_file <- tempfile(fileext = ".rdata")
download.file(url, destfile = temp_file, mode = "wb")
load(temp_file)

robust_cov_a <- vcovHC(model_a, type = "HC1")
se_kids_robust <- sqrt(diag(robust_cov_a))["kids"]
beta_kids_a <- coef(model_a)["kids"]
t_crit <- qt(0.975, df = model_a$df.residual)

ci_lower_d <- beta_kids_a - t_crit * se_kids_robust
ci_upper_d <- beta_kids_a + t_crit * se_kids_robust

cat("--- (d) 穩健 OLS 結果 ---\n")
cat("95% CI: [", ci_lower_d, ",", ci_upper_d, "]\n\n")