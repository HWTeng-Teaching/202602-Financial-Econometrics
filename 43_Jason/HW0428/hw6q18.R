# Chan Nok Hang 414707007
#hw6 ch10-Q18

library(POE5Rdata)
data("mroz")
mroz_sub <- subset(mroz, lfp == 1)

# Part a
mroz_sub$MOTHERCOLL <- ifelse(mroz_sub$mothereduc > 12, 1, 0)
mroz_sub$FATHERCOLL <- ifelse(mroz_sub$fathereduc > 12, 1, 0)

# 1. Standard Interpretation: Separate percentages
pct_mother <- mean(mroz_sub$MOTHERCOLL) * 100
pct_father <- mean(mroz_sub$FATHERCOLL) * 100

# 2. "Both" Interpretation (What you asked about): Both parents went to college
pct_both <- mean(mroz_sub$MOTHERCOLL == 1 & mroz_sub$FATHERCOLL == 1) * 100

# 3. "At least one" Interpretation: Either mother OR father went to college
pct_either <- mean(mroz_sub$MOTHERCOLL == 1 | mroz_sub$FATHERCOLL == 1) * 100
cat('mother:', pct_mother, 'father:', pct_father, 'both:', pct_both, 'either:', pct_either)

# Part b
# Calculate the correlation matrix
cor_matrix <- cor(mroz_sub[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print(cor_matrix)

# Part c
# Estimate 2SLS
mroz_sub$expersq <- mroz_sub$exper**2
iv_model_c <- ivreg(log(wage) ~ educ + exper + expersq | MOTHERCOLL + exper + expersq, data = mroz_sub)
# Display the regression results to see the coefficient for educ
summary(iv_model_c)

# Get 95% Confidence Interval for EDUC
confint(iv_model_c, "educ", level = 0.95)

# Part d
# 1. Estimate the first-stage OLS regression
first_stage_d <- lm(educ ~ MOTHERCOLL + exper + expersq, data = mroz_sub)

# Display the summary to see the coefficients
summary(first_stage_d)

# 2. Conduct the F-test specifically for MOTHERCOLL
# Make sure the 'car' package is loaded for linearHypothesis
library(car)
linearHypothesis(first_stage_d, "MOTHERCOLL = 0")

# Part e
# Estimate 2SLS with both MOTHERCOLL and FATHERCOLL as instruments
iv_model_e <- ivreg(log(wage) ~ educ + exper + expersq | MOTHERCOLL + FATHERCOLL + exper + expersq, data = mroz_sub)

# View the results
summary(iv_model_e)

# Get the new 95% Confidence Interval for EDUC
confint(iv_model_e, "educ", level = 0.95)

# Part f
# 1. Estimate the first-stage OLS regression with both instruments
first_stage_f <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + expersq, data = mroz_sub)

# Display the summary
summary(first_stage_f)

# 2. Test the joint significance of both instruments
library(car)
linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

# Part g
# The AER package will automatically calculate the Sargan test for overidentified models
summary(iv_model_e, diagnostics = TRUE)
