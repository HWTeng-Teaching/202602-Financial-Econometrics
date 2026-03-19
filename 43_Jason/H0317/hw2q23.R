# Chan Nok Hang 414707007
#hw2 ch3-Q23

# Load the data
library(POE5Rdata)
data(collegetown)

# Create the squared variable and run the model
collegetown$sqft2 <- (collegetown$sqft)^2
model <- lm(price ~ sqft2, data=collegetown)

# Extract coefficient and standard error
a2 <- coef(model)["sqft2"]
se_a2 <- coef(summary(model))["sqft2", "Std. Error"]

# Calculate t-statistic and p-value
t_stat <- (40 * a2 - 13) / (40 * se_a2)
p_value <- pt(t_stat, df = 498, lower.tail = FALSE)

# Print results
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

# Part c
# A 2000 sq ft house means SQFT = 20, so our squared term is 20^2 = 400
new_house <- data.frame(sqft2 = 400)

# Calculate the expected value and the 95% confidence interval
prediction <- predict(model, newdata = new_house, interval = "confidence", level = 0.95)

# Print the exact values
print(prediction)

# part d
# Filter the dataset for houses with exactly 2000 sq ft (remember to use lowercase)
houses_2000 <- subset(collegetown, sqft == 20)

# Calculate the sample mean of their prices
mean_price_2000 <- mean(houses_2000$price)

# Count how many houses there are
n_houses <- nrow(houses_2000)

# Print the results
cat("Number of houses with 2000 sq ft:", n_houses, "\n")
cat("Sample mean price:", mean_price_2000, "\n")