library(POE5Rdata)
# 安裝套件 (僅需執行一次)
install.packages("AER") 
# 載入套件 (每次重新開啟 R 都需執行)
library(AER)

# part a

# Select women who participated in the labor force
mroz_working <- subset(mroz, lfp == 1)

# Create two new variables：MOTHERCOLL & FATHERCOLL
mroz_working$mothercoll <- ifelse(mroz_working$mothereduc > 12, 1, 0)
mroz_working$fathercoll <- ifelse(mroz_working$fathereduc > 12, 1, 0)

# Calculate percentage
mean(mroz_working$mothercoll)
mean(mroz_working$fathercoll)
#------------------------------------------------------------------------
# part b

# Create a correlation matrix of EDUC, MOTHERCOLL, and FATHERCOLL
cor(mroz_working[c("educ", "mothercoll", "fathercoll")])
#------------------------------------------------------------------------
# part c

# Create a IV regression model (IV: mothercoll)
mroz1_iv <- ivreg(log(wage)~educ + exper + I(exper^2)|exper + I(exper^2) + mothercoll, data = mroz_working)

# See the result
summary(mroz1_iv)

# Calculate the 95% CI for educ
confint(mroz1_iv, "educ",level = 0.95)
#------------------------------------------------------------------------
#------------------------------------------------------------------------
# part d

# First stage regression
first_stage_m <- lm(educ~exper + I(exper^2) + mothercoll, data = mroz_working)

# Estimate the first stage equation
summary(first_stage_m)

# Conduct the hypothesis test
linearHypothesis(first_stage_m, "mothercoll=0")
#------------------------------------------------------------------------


# part e

# Create a IV regression model (IV: mothercoll+fathercoll)
mroz2_iv <- ivreg(log(wage)~educ + exper + I(exper^2)|exper + I(exper^2) + mothercoll + fathercoll, data = mroz_working)

# See the result
summary(mroz2_iv)

# Calculate the 95% CI for educ
confint(mroz2_iv, "educ",level = 0.95)
#------------------------------------------------------------------------

# part f

# First stage regression
first_stage_mf <- lm(educ~exper + I(exper^2) + mothercoll + fathercoll, data = mroz_working)

# Estimate the first stage equation
summary(first_stage_mf)

# Conduct the hypothesis test
linearHypothesis(first_stage_mf, c("mothercoll=0", "fathercoll=0"))
#------------------------------------------------------------------------

# part g

# Perform weak instruments, Hausman, and Sargan tests
summary(mroz2_iv, diagnostics=TRUE)


