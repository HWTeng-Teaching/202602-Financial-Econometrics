library(lmtest)
library(sandwich)

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
temp_file <- tempfile(fileext = ".rdata")
download.file(url, destfile = temp_file, mode = "wb")
load(temp_file)



# 假設變異數與 income^2 成正比，權重為 1/income^2
gls_weights <- 1 / (vacation$income^2)
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = gls_weights)

# 1. 傳統 GLS 信賴區間
ci_gls <- confint(model_gls, "kids", level = 0.95)
ci_lower_e_trad <- ci_gls[1]
ci_upper_e_trad <- ci_gls[2]

# 2. 穩健 GLS 信賴區間
robust_cov_gls <- vcovHC(model_gls, type = "HC1")
se_kids_gls_robust <- sqrt(diag(robust_cov_gls))["kids"]
beta_kids_gls <- coef(model_gls)["kids"]

ci_lower_e_robust <- beta_kids_gls - t_crit * se_kids_gls_robust
ci_upper_e_robust <- beta_kids_gls + t_crit * se_kids_gls_robust

cat("--- (e) GLS 結果 ---\n")
cat("傳統 GLS 95% CI: [", ci_lower_e_trad, ",", ci_upper_e_trad, "]\n")
cat("穩健 GLS 95% CI: [", ci_lower_e_robust, ",", ci_upper_e_robust, "]\n")