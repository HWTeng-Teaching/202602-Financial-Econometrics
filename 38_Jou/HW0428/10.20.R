library(AER)

url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
destfile <- tempfile(fileext = ".rdata")
download.file(url, destfile, mode = "wb")
load(destfile)

capm5$y  <- capm5$msft - capm5$riskfree
capm5$x  <- capm5$mkt  - capm5$riskfree

#(a) OLS CAPM
ols <- lm(y ~ x, data = capm5)
summary(ols)

coef(ols)
cat("OLS beta =", beta_ols, "\n")

# (b) RANK IV , First Stage
capm5$rank <- rank(capm5$x)

fs1 <- lm(x ~ rank, data = capm5)
summary(fs1)

# (c) Hausman test using first-stage residual from (b)
capm5$vhat <- residuals(fs1)

c_aug <- lm(y ~ x + vhat, data = capm5)
summary(c_aug)

# (d) IV / 2SLS using RANK
iv1 <- ivreg(y ~ x | rank, data = capm5)
summary(iv1)

# (e) 加入 POS
capm5$pos <- ifelse(capm5$x > 0, 1, 0)

fs2 <- lm(x ~ rank + pos, data = capm5)
summary(fs2)

# (f) Hausman test
capm5$vhat2 <- residuals(fs2)

hausman <- lm(y ~ x + vhat2, data = capm5)
summary(hausman)

# (g) IV / 2SLS（RANK + POS）
iv2 <- ivreg(y ~ x | rank + pos, data = capm5)
summary(iv2)

# (h) Sargan test（NR^2）
capm5$uhat <- residuals(iv2)

sargan_reg <- lm(uhat ~ rank + pos, data = capm5)
R2 <- summary(sargan_reg)$r.squared

n <- nrow(capm5)
NR2 <- n * R2
NR2

p_value <- 1 - pchisq(NR2, df = 1)
p_value