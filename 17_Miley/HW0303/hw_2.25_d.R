# we need to load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/cex5_small.rdata")

# we only retain households whose foodaway are greater than 0
valid_data <- subset(cex5_small, foodaway > 0)

# the log-linear regression ln(FOODAWAY) = beta1 + beta2 INCOME +e
income_model <- lm(log(foodaway) ~ income, data = valid_data)
b1 <- coef(income_model)[[1]]
b2 <- coef(income_model)[[2]]

# the estimated slope is 0.0069
# it indicates that for every additional unit increase in monthly income, the monthly expenditure on food away from home is estimated to increase by 0.69%.

# the intercept is 3.129