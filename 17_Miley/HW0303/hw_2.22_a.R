# load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/star5_small.rdata")

# we need to create a new dataset to extract data without aide
# compare "regular classes" and "small classes"
without_aide <- subset(star5_small, aide==0)

# build the linear regression model using the lm function: TOTALSCORE = beta1 + beta2 SMALL + e
size_model <- lm(totalscore ~ small, data = without_aide)

# find the coefficient of the model
b1 <- coef(size_model)[[1]]
b2 <- coef(size_model)[[2]]

# b1(intercept): This is the average score for students in a regular-sized class.
# b2(slope): It indicates the estimated difference in scores for students in small classes.









