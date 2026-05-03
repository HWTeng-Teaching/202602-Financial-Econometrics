library(PoEdata)
data("mroz")

mroz_work <- subset(mroz, lfp == 1)

# (a) Create dummy variables
mroz_work$mothercoll <- ifelse(mroz_work$mothereduc > 12, 1, 0)
mroz_work$fathercoll <- ifelse(mroz_work$fathereduc > 12, 1, 0)

pct_mother <- mean(mroz_work$mothercoll) * 100
pct_father <- mean(mroz_work$fathercoll) * 100

cat("Percentage of mothers with some college:", pct_mother, "%\n")
cat("Percentage of fathers with some college:", pct_father, "%\n")

# (b) Correlations
corr <- cor(mroz_work[, c("educ", "mothercoll", "fathercoll")])
print(corr)

cor(mroz_work[, c("educ","mothereduc", "fathereduc")])

# (c) IV Estimation
iv1 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll + exper + I(exper^2), data = mroz_work)
summary(iv1)

print(confint(iv1, "educ", level = 0.95))

# (d) First-stage regression for (c)
fs1 <- lm(educ ~ mothercoll + exper + I(exper^2), data = mroz_work)
summary(fs1)

fs1_r <- lm(educ ~ exper + I(exper^2), data = mroz_work)

print(anova(fs1_r, fs1))

# (e) IV using mothercoll and fathercoll
iv2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll + fathercoll + exper + I(exper^2), data = mroz_work )
summary(iv2)

confint(iv2, "educ", level = 0.95)

# (f) First-stage regression for (e)
fs2 <- lm(educ ~ mothercoll + fathercoll + exper + I(exper^2),  data = mroz_work)
summary(fs2)

fs2_r <- lm(educ ~ exper + I(exper^2),  data = mroz_work)

print(anova(fs2_r, fs2))

# (g) Overidentification test
print(summary(iv2, diagnostics = TRUE))
