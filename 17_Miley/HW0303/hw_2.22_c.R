# load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/star5_small.rdata")

# We need to exclude small classes
regular_data <- subset(star5_small, small == 0)

# fit the model to the data: TOTALSCORE = gamma1 + gamma2 AIDE + e
aide_model <- lm(totalscore ~ aide, data = regular_data)

# find the coefficient of the model
r1 <- coef(aide_model)[[1]]
r2 <- coef(aide_model)[[2]]


