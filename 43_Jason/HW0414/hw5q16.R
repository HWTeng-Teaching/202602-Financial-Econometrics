# Chan Nok Hang 414707007
#hw5 ch8-Q16

library(POE5Rdata)
data("vacation")

# Part a
# Assuming the POE5 vacation dataset is loaded into the dataframe 'vacation'
model_a <- lm(miles ~ income + age + kids, data = vacation)

# View the full regression output
summary(model_a)

# Extract the 95% confidence interval for KIDS
confint(model_a, "kids", level = 0.95)

# Part b
# Extract the residuals from the OLS model
ols_residuals <- resid(model_a)

# Plot residuals vs. INCOME
plot(vacation$income, ols_residuals, 
     main = "OLS Residuals vs. Income", 
     xlab = "Income", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red", lwd = 2)

# Plot residuals vs. AGE
plot(vacation$age, ols_residuals, 
     main = "OLS Residuals vs. Age", 
     xlab = "Age", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red", lwd = 2)
#plot(vacation$income, resid(model_a))
#plot(vacation$age, resid(model_a))

# Part c
# 1. Sort the dataframe by INCOME in ascending order
vacation_sorted <- vacation[order(vacation$income), ]

# 2. Run OLS on the first 90 observations (Low Income)
model_low <- lm(miles ~ income + age + kids, data = vacation_sorted[1:90, ])

# 3. Run OLS on observations 111 to 200 (High Income)
model_high <- lm(miles ~ income + age + kids, data = vacation_sorted[111:200, ])

# 4. Calculate the Sum of Squared Errors (SSE) for both
sse_low <- sum(resid(model_low)^2)
sse_high <- sum(resid(model_high)^2)

# 5. Calculate the F-statistic (High variance over Low variance)
F_stat <- sse_high / sse_low
F_stat

# Calculate the 5% critical value (upper tail)
qf(0.95, df1 = 86, df2 = 86)

# Part d
# Load the necessary libraries
library(lmtest)
library(sandwich)

# 1. View the regression results with robust standard errors (HC1)
coeftest(model_a, vcov = vcovHC(model_a, type = "HC1"))

# 2. Extract the robust 95% confidence interval for all variables
robust_ci <- coefci(model_a, vcov = vcovHC(model_a, type = "HC1"), level = 0.95)

# 3. View just the interval for 'kids'
robust_ci["kids", ]

# Part e
# 1. Run the GLS model using the weights argument
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = 1/(income^2))

# 2. View the regression summary
summary(model_gls)

# 3. Extract the 95% confidence interval for 'kids'
confint(model_gls, "kids", level = 0.95)
