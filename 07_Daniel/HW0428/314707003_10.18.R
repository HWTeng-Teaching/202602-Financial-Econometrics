rm(list=ls())
#install.packages("AER")
library(POE5Rdata)
library(AER)
library(car)
data = mroz

data("mroz")

dat <- subset(mroz, lfp == 1)

# (a)
dat$mothercoll <- ifelse(dat$mothereduc > 12, 1, 0)
dat$fathercoll <- ifelse(dat$fathereduc > 12, 1, 0)

cat("Sample size:", nrow(dat), "\n")
cat("Mother some college percentage:", mean(dat$mothercoll) * 100, "\n")
cat("Father some college percentage:", mean(dat$fathercoll) * 100, "\n")

table(dat$mothercoll)
table(dat$fathercoll)

# (b)
cor(dat[, c("educ", "mothercoll", "fathercoll")])

# optional comparison
cor(dat[, c("educ", "mothereduc", "fathereduc")])

# (c)
iv_c <- ivreg(
  log(wage) ~ educ + exper + I(exper^2) |
     exper + I(exper^2) + mothercoll,
  data = dat
)

summary(iv_c)
confint(iv_c, "educ", level = 0.95)

# (d)
fs_d <- lm(
  educ ~ mothercoll + exper + I(exper^2),
  data = dat
)

summary(fs_d)
linearHypothesis(fs_d, "mothercoll = 0")

t_mothercoll <- summary(fs_d)$coefficients["mothercoll", "t value"]
cat("First-stage F in part (d):", t_mothercoll^2, "\n")

# (e)
iv_e <- ivreg(
  log(wage) ~ educ + exper + I(exper^2) |
    mothercoll + fathercoll + exper + I(exper^2),
  data = dat
)

summary(iv_e)
confint(iv_e, "educ", level = 0.95)

# (f)
fs_f <- lm(
  educ ~ mothercoll + fathercoll + exper + I(exper^2),
  data = dat
)

summary(fs_f)

linearHypothesis(
  fs_f,
  c("mothercoll = 0", "fathercoll = 0")
)

# (g)
summary(iv_e, diagnostics = TRUE)