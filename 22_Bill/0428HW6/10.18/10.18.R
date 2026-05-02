load("/Users/liaochenghao/Desktop/Financial Econometrics/Homework/HW6/Data/mroz.rdata")
table(mroz$lfp) # confirm variable
df <- subset(mroz, lfp == 1)
names(df)

# a.

# create dummy variables
df$mothercoll <- ifelse(df$mothereduc > 12, 1, 0)
df$fathercoll <- ifelse(df$fathereduc > 12, 1, 0)

# calculate the percentage
# na.rm can add or not add since the data don't have missing value
mean(df$mothercoll, na.rm = TRUE)
mean(df$fathercoll)

# b.
vars <- df[, c("educ", "mothercoll", "fathercoll")]
# complete.obs: only use "no NA observation value" to calculate correlations
cor(vars, use = "complete.obs")

# c.
# construct variable "EXPER^2"
df$exper2 <- df$exper ^ 2

# 2SLS estimation
iv_model_c <- ivreg(log(wage) ~ exper + exper2 + educ |
                    exper + exper2 + mothercoll,
                  data = df)
summary(iv_model_c)
coef(iv_model_c)["educ"]
sqrt(vcov(iv_model_c)["educ", "educ"])

# 95% C.I.
b <- coef(iv_model_c)["educ"]
se <- sqrt(vcov(iv_model_c)["educ", "educ"])

lower <- b - 1.96 * se
upper <- b + 1.96 * se

c(lower, upper)

# d.
# First stage regression
fs_d <- lm(educ ~ exper + exper2 + mothercoll, data = df)
summary(fs_d)

# Find F-stat
linearHypothesis(fs_d, "mothercoll = 0")

# e.
iv_model_e <- ivreg(log(wage) ~ exper + exper2 + educ |
                      exper + exper2 + mothercoll + fathercoll,
                    data = df)
summary(iv_model_e)

b <- coef(iv_model_e)["educ"]
se <- sqrt(vcov(iv_model_e)["educ", "educ"])

lower <- b - 1.96 * se
upper <- b + 1.96 * se

c(lower, upper)

# f.
# First stage regression
fs_f <- lm(educ ~ exper + exper2 + mothercoll + fathercoll, data = df)
summary(fs_f)

# Find F-stat
linearHypothesis(fs_f, c("mothercoll = 0", "fathercoll = 0"))

# g.
summary(iv_model_e, diagnostics = TRUE)
