# compare how an aide affects reading and math scores seperately

# load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/star5_small.rdata")

# We need to create a new dataset to exclude small classes
regular_data <- subset(star5_small, small == 0)

# fit the model to the data: READSCORE = beta1 + beta2 AIDE + e
read_aide_model <- lm(readscore ~ aide, data = regular_data)

# find the coefficient of the model
b1 <- coef(read_aide_model)[[1]]
b2 <- coef(read_aide_model)[[2]]

# b1: 432.665
# b2: 2.871

# fit the model to the data: MATHSCORE = alpha1 + alpha2 AIDE + e
math_aide_model <- lm(mathscore ~ aide, data = regular_data)

# find the coefficient of the model
a1 <- coef(math_aide_model)[[1]]
a2 <- coef(math_aide_model)[[2]]

# a1: 483.777
# a2: 1.435

# an aide have a larger impact on reading than on math
