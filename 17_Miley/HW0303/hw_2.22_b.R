# See if small classes affect reading and math differently

# load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/star5_small.rdata")

# we need to create a new dataset to extract data without aide
without_aide <- subset(star5_small, aide==0)

# fit the model to the data: READSCORE = beta1 + beta2 SMALL + e
read_model <- lm(readscore ~ small, data = without_aide)

# find the coefficient of the model
b1 <- coef(read_model)[[1]]
b2 <- coef(read_model)[[2]]

# b1: 432.665
# b2: 6.924

# fit the model to the data: MATHSCORE = alpha1 + alpha2 SMALL + e
math_model <- lm(mathscore ~ small, data = without_aide)

# find the coefficient of the model
a1 <- coef(math_model)[[1]]
a2 <- coef(math_model)[[2]]

# a1: 483.777
# a2: 5.251

# Small classes seem to have a larger impact on reading than on math.
