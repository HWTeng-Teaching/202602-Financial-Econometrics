# Chan Nok Hang 414707007
#Q25
library(POE5Rdata)
data("cex5_small")

# --- Part (a): Summary Stats & Histogram for FOODAWAY ---
# Histogram
hist(cex5_small$foodaway, 
     main="Histogram of Food Away From Home", 
     xlab="Monthly Expenditure per Person ($)", 
     col="lightblue")

# Summary statistics (Mean, Median, 25th, 75th percentiles)
summary(cex5_small$foodaway)
# To specifically isolate percentiles:
quantile(cex5_small$foodaway, probs = c(0.25, 0.5, 0.75))

# --- Part (b): Grouping by Education ---
# Assuming standard dummy variables 'advanced' and 'college'
# 1. Advanced Degree
adv_data <- subset(cex5_small, advanced == 1)
cat("Advanced - Mean:", mean(adv_data$foodaway), "Median:", median(adv_data$foodaway), "\n")

# 2. College Degree (Assuming this means College but NO Advanced)
# Note: Check your dataset to see if 'college' means "highest degree is college"
col_data <- subset(cex5_small, college == 1 & advanced == 0)
cat("College - Mean:", mean(col_data$foodaway), "Median:", median(col_data$foodaway), "\n")

# 3. No Advanced or College Degree
none_data <- subset(cex5_small, advanced == 0 & college == 0)
cat("None - Mean:", mean(none_data$foodaway), "Median:", median(none_data$foodaway), "\n")

# --- Part (c): The Log Transformation ---
# We must filter out zeros because log(0) is mathematically undefined (returns -Inf in R)
cex5_filtered <- subset(cex5_small, foodaway > 0)
cex5_filtered$ln_foodaway <- log(cex5_filtered$foodaway)

# Histogram of ln(FOODAWAY)
hist(cex5_filtered$ln_foodaway, 
     main="Histogram of ln(FOODAWAY)", 
     xlab="ln(Expenditure)", 
     col="lightgreen")

# Summary stats for the log-transformed variable
summary(cex5_filtered$ln_foodaway)

# --- Part (d): The Log-linear regression model ---
# Estimate the log-linear model
model_d <- lm(ln_foodaway ~ income, data = cex5_filtered)
summary(model_d)

# --- Part (e): Plotting Data and Fitted Line ---
# 1. Plot ln(FOODAWAY) against INCOME
plot(cex5_filtered$income, cex5_filtered$ln_foodaway,
     main = "ln(FOODAWAY) vs INCOME",
     xlab = "Household Monthly Income ($100 units)",
     ylab = "ln(Food Away Expenditure)",
     col = "blue", 
     pch = 20, # Small solid circles
     cex = 0.5) # Make points smaller to handle dense data

# 2. Add the fitted regression line
abline(model_d, col = "red", lwd = 2)

# --- Part (f): Plotting the Residuals ---
# 1. Extract residuals from your model
res_d <- residuals(model_d)

# 2. Plot residuals vs INCOME
plot(cex5_filtered$income, res_d,
     main = "Residuals vs INCOME",
     xlab = "Household Monthly Income ($100 units)",
     ylab = "Least Squares Residuals",
     col = "darkgreen",
     pch = 20,
     cex = 0.5)

# 3. Add a horizontal line at 0 for reference
abline(h = 0, col = "red", lwd = 2, lty = 2) # lty=2 makes it dashed