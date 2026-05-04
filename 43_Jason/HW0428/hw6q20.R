# Chan Nok Hang 414707007
#hw6 ch10-Q20

library(POE5Rdata)
data("capm5")

#Part a
# 1. Create the excess return variables
capm5$msft_ex <- capm5$msft - capm5$riskfree
capm5$mkt_ex <- capm5$mkt - capm5$riskfree

# 2. Estimate the CAPM using OLS
ols_model_a <- lm(msft_ex ~ mkt_ex, data = capm5)

# 3. View the results
summary(ols_model_a)

# Part b
# 1. Create the RANK variable
# The rank() function in R handles this perfectly. By default, it sorts smallest to largest.
capm5$RANK <- rank(capm5$mkt_ex)

# 2. Obtain the first-stage regression results
# We regress the endogenous variable (mkt_ex) on our instrument (RANK)
first_stage_b <- lm(mkt_ex ~ RANK, data = capm5)

# 3. View the results to check significance and strength
summary(first_stage_b)

# Part c
# 1. Compute the first-stage residuals (\hat{v})
capm5$v_hat <- resid(first_stage_b)

# 2. Estimate the augmented CAPM equation by OLS
augmented_model_c <- lm(msft_ex ~ mkt_ex + v_hat, data = capm5)

# 3. View the results to check the significance of v_hat
summary(augmented_model_c)

# Part d
library(AER)

# Estimate the CAPM using RANK as the IV
iv_model_d <- ivreg(msft_ex ~ mkt_ex | RANK, data = capm5)

# View the results
summary(iv_model_d)

# Part e
# 1. Create the POS dummy variable (1 if positive, 0 otherwise)
capm5$POS <- ifelse(capm5$mkt_ex > 0, 1, 0)

# 2. Obtain the new first-stage regression results
first_stage_e <- lm(mkt_ex ~ RANK + POS, data = capm5)
summary(first_stage_e)

# 3. Test joint significance using linearHypothesis
library(car)
linearHypothesis(first_stage_e, c("RANK = 0", "POS = 0"))

# Part f
# 1. Extract the residuals from the new first-stage regression from part (e)
capm5$v_hat_e <- resid(first_stage_e) 

# 2. Estimate the augmented CAPM equation using OLS
augmented_model_f <- lm(msft_ex ~ mkt_ex + v_hat_e, data = capm5)

# 3. View the results to check the significance
summary(augmented_model_f)

# Part g
# Estimate the CAPM using RANK and POS as IVs
iv_model_g <- ivreg(msft_ex ~ mkt_ex | RANK + POS, data = capm5)

# View the results
summary(iv_model_g)

# Part h
# 1. Extract IV residuals from part (g)
capm5$iv_res <- resid(iv_model_g)

# 2. Regress IV residuals on the instruments
sargan_reg <- lm(iv_res ~ RANK + POS, data = capm5)

# 3. Calculate the test statistic (n * R-squared)
# n is the number of observations, which is 180 for the capm5 dataset
n <- nrow(capm5) 
r_squared <- summary(sargan_reg)$r.squared
sargan_stat <- n * r_squared

# 4. Calculate the p-value from a Chi-Square distribution (df = 1)
p_value <- pchisq(sargan_stat, df = 1, lower.tail = FALSE)

# Display the test statistic and p-value
cat("Sargan Test Statistic:", sargan_stat, "\n")
cat("p-value:", p_value, "\n")
