load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW6/Data/capm5.rdata")
df <- capm5

# a.
df$excess_msft <- df$msft - df$riskfree
df$excess_mkt <- df$mkt - df$riskfree

m_a <- lm(excess_msft ~ excess_mkt, data = df)
summary(m_a)

# b.
df$excess_mkt_rank <- rank(df$excess_mkt)
m_b <- lm(excess_msft ~ excess_mkt_rank, data = df)
summary(m_b)

summary(m_b)$r.squared

# c.
m_d <- lm(excess_mkt ~ excess_mkt_rank, data = df)
df$v_hat <- residuals(m_d)

# augmented regression
aug_model <- lm(excess_msft ~ excess_mkt + v_hat, data = df)
summary(aug_model)

# d.
iv_model <- ivreg(excess_msft ~ excess_mkt | excess_mkt_rank, data = df)
summary(iv_model)

# e.
df$pos <- ifelse(df$excess_mkt > 0, 1, 0)
m_e <- lm(excess_mkt ~ excess_mkt_rank + pov, data = df)
summary(m_e)
summary(m_e)$fstatistic
summary(m_e)$r.squared

# f.
# first stage (include 2 IVs)
df$v_hat_f <- residuals(m_e)

# aug model
aug_model_f <- lm(excess_msft ~ excess_mkt + v_hat_f, data = df)
summary(aug_model_f)

# g.
# 2SLS
iv_model_g <- ivreg(excess_msft ~ excess_mkt | excess_mkt_rank + pos, data = df)
summary(iv_model_g)

# h.
df$u_hat <- residuals(iv_model_g)

# Use IV to do residuals regression
sargan_reg <- lm(u_hat ~ excess_mkt_rank + pos, data = df)
summary(sargan_reg)

# Sargan t-stat
n <- nrow(df)
R2 <- summary(sargan_reg)$r.squared
J <- n * R2
J

# Hypothesis test
p_value <- 1 - pchisq(J, df = 1)
p_value
