library(POE5Rdata)
library(AER)
#-------------------------------------------------------------------
#part a

# Calculate msft's risk premium
capm5$y_msft <- capm5$msft - capm5$riskfree

# Caculate mkt's risk premium
capm5$x_mkt <- capm5$mkt - capm5$riskfree

# linear regression model
msft_ols <- lm(y_msft~x_mkt, data = capm5)

# See the result
summary(msft_ols)

#-------------------------------------------------------------------
#part b

# sort (rm-rf) from smallest to largest
capm5$rank <- rank(capm5$x_mkt)

# first stage regression
first_stage_rank <- lm(x_mkt~rank, data = capm5)
summary(first_stage_rank)
#-------------------------------------------------------------------
#part c

# Compute the first stage residuals
capm5$v_hat <- resid(first_stage_rank)

# Estimate the augmented equation
augmented_model <- lm(y_msft~x_mkt + v_hat, data = capm5)
summary(augmented_model)
#-------------------------------------------------------------------
#part d

# 2SLS model: use rank as an IV
rank_iv <- ivreg(y_msft~x_mkt | rank, data = capm5)
summary(rank_iv)
#-------------------------------------------------------------------
#part e

# Create a new variable: POS
capm5$pos <- ifelse(capm5$x_mkt > 0, 1, 0)

# first stage regression with rank and pos
first_stage_rank_pos <- lm(x_mkt~rank + pos, data = capm5)
summary(first_stage_rank_pos)
#-------------------------------------------------------------------
#part f

# Compute the first stage residuals
capm5$v_hat_2 <- resid(first_stage_rank_pos)

# Estimate the augmented equation
augmented_model_2 <- lm(y_msft~x_mkt + v_hat_2, data = capm5)
summary(augmented_model_2)
#-------------------------------------------------------------------
#part g

# 2SLS model: use rank and pos as IVs
rank_pos_iv <- ivreg(y_msft~x_mkt | rank + pos, data = capm5)
summary(rank_pos_iv)
#-------------------------------------------------------------------
#part h

# Obtain the IV/2SLS residuals
e_hat_iv <- resid(rank_pos_iv)

# Sargan test: see whether rank and pos can predict error
aux_model <- lm(e_hat_iv~rank + pos, data = capm5)
summary(aux_model)

# R^2
r2 <- summary(aux_model)$r.squared

# Calculate NxR^2
N <- nrow(capm5)
sargan_statistic <- N*r2
print(sargan_statistic)

# Chi-Square test
#df = L-B = 1
# Calculate the critical value
critical_value <- qchisq(0.95, df = 1)

if (sargan_statistic > critical_value){
  cat("Reject H0: at least one of the IVs is correlated with error")
} else{
  cat("Fail to reject H0: The two IVs are valid!")
}


