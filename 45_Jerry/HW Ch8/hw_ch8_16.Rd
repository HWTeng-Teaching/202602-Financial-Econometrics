library(POE5Rdata)
data(vacation)
?vacation

# Problem a
mod1 <- lm(miles ~ income + age + kids,data=vacation)
smod1 <- summary(mod1)
smod1
b4 <- coef(smod1)[4,1]
se4 <- coef(smod1)[4,2]
se4
alpha <- 0.05
df = df.residual(mod1)
tcr <- qt(1-alpha/2,df)
lowb4 <- b4 - tcr*se4
upb4 <- b4 + tcr*se4
cat("95% CI for beta4: [", lowb4, ",", upb4, "]\n")


# Problem b
ehat <- resid(mod1)
plot(vacation$income,ehat)
plot(vacation$age,ehat)

# Problem c

vacation_sorted <- vacation[order(vacation$income), ]

mod_low  <- lm(miles ~ income + age + kids, data = vacation_sorted[1:90, ])
mod_high <- lm(miles ~ income + age + kids, data = vacation_sorted[111:200, ])

SSE1 <- sum(resid(mod_low)^2)
SSE2 <- sum(resid(mod_high)^2)
df1  <- df.residual(mod_low)
df2  <- df.residual(mod_high)

GQ_stat <- (SSE2 / df2) / (SSE1 / df1)
GQ_stat

p_value_GQ <- pf(GQ_stat, df2, df1, lower.tail = FALSE)
p_value_GQ

# Problem d
# install.packages("car")
# install.packages("lmtest")
library(car)
library(lmtest)

cov1 <- hccm(mod1, type = "hc1")
robust_result <- coeftest(mod1, vcov.= cov1)
robust_result

b4_robust  <- robust_result["kids", "Estimate"]
se4_robust <- robust_result["kids", "Std. Error"]

tcr <- qt(1-alpha/2, df.residual(mod1))

low_d <- b4_robust - tcr * se4_robust
up_d  <- b4_robust + tcr * se4_robust
cat("Robust 95% CI for beta4: [", low_d, ",", up_d, "]\n")


# Problem e

n <- nrow(vacation)

vacation$miles_w    <- vacation$miles   / vacation$income  # y*
vacation$inv_income <- 1                / vacation$income  # x1*
vacation$age_w      <- vacation$age     / vacation$income  # x3*
vacation$kids_w     <- vacation$kids    / vacation$income  # x4*

mod_gls <- lm(miles_w ~ inv_income + age_w + kids_w, data = vacation)
summary(mod_gls)

# --- conventional GLS ---
gls_coef <- coef(summary(mod_gls))
b4_gls   <- gls_coef["kids_w", "Estimate"]
se4_gls  <- gls_coef["kids_w", "Std. Error"]

tcr_gls <- qt(1-alpha/2, df.residual(mod_gls))

low_e_conv <- b4_gls - tcr_gls * se4_gls
up_e_conv  <- b4_gls + tcr_gls * se4_gls
cat("GLS conventional 95% CI for beta4: [", low_e_conv, ",", up_e_conv, "]\n")

# --- Robust GLS ---
cov_gls <- hccm(mod_gls, type = "hc1")
robust_gls <- coeftest(mod_gls, vcov.= cov_gls)
b4_gls_r   <- robust_gls["kids_w", "Estimate"]
se4_gls_r  <- robust_gls["kids_w", "Std. Error"]

low_e_rob <- b4_gls_r - tcr_gls * se4_gls_r
up_e_rob  <- b4_gls_r + tcr_gls * se4_gls_r
cat("GLS robust    95% CI for beta4: [", low_e_rob, ",", up_e_rob, "]\n")

cat("(a) OLS conventional: [", lowb4,    ",", upb4,    "]\n")
cat("(d) OLS robust:       [", low_d,    ",", up_d,    "]\n")
cat("(e) GLS conventional: [", low_e_conv,",", up_e_conv,"]\n")
cat("(e) GLS robust:       [", low_e_rob, ",", up_e_rob, "]\n")
