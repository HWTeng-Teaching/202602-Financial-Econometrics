library(POE5Rdata)
library(AER)  # for ivreg
data(mroz)

mroz1 <- mroz[mroz$lfp == 1, ]
nrow(mroz1)

# ── Part a ────────────────────────────────
mroz1$MOTHERCOLL <- as.integer(mroz1$mothereduc > 12)
mroz1$FATHERCOLL <- as.integer(mroz1$fathereduc > 12)

mean(mroz1$MOTHERCOLL)
mean(mroz1$FATHERCOLL)
mean(mroz1$MOTHERCOLL == 1 | mroz1$FATHERCOLL == 1)

# ── Part b ────────────────────────────────
cor(mroz1[, c("educ", "MOTHERCOLL", "FATHERCOLL")])

# ── Part c：IV: MOTHERCOLL ────────────────────────────────────
iv_c <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                exper + I(exper^2) + MOTHERCOLL,
              data = mroz1)
summary(iv_c)
confint(iv_c, "educ", level = 0.95)

# ── Part d: First stage equation ────────────────────────────────
library(car)
first_d <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL, data = mroz1)
summary(first_d)
linearHypothesis(first_d, "MOTHERCOLL = 0")

# ── Part e: IV: MOTHERCOLL + FATHERCOLL ───────────────────────
iv_e <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                exper + I(exper^2) + MOTHERCOLL + FATHERCOLL,
              data = mroz1)
summary(iv_e)
confint(iv_e, "educ", level = 0.95)

# ── Part f: First stage equation ──────────────────────────────────
first_f <- lm(educ ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz1)
summary(first_f)
linearHypothesis(first_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

# ── Part g: Sargan test ──────────────────────────────────────────
mroz1$ehat <- residuals(iv_e)
sargan_reg <- lm(ehat ~ exper + I(exper^2) + MOTHERCOLL + FATHERCOLL, data = mroz1)
n <- nrow(mroz1)
R2 <- summary(sargan_reg)$r.squared
sargan_stat <- n * R2
sargan_stat
p_value <- 1 - pchisq(sargan_stat, df = 1)  # df = 超額工具數 = 2-1 = 1
p_value