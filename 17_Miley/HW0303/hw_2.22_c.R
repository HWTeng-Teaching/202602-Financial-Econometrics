# see if adding an aide helps students in regular-sized classes.

# load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/star5_small.rdata")

# We need to create a new dataset to exclude small classes
regular_data <- subset(star5_small, small == 0)

# fit the model to the data: TOTALSCORE = gamma1 + gamma2 AIDE + e
aide_model <- lm(totalscore ~ aide, data = regular_data)

# find the coefficient of the model
r1 <- coef(aide_model)[[1]]
r2 <- coef(aide_model)[[2]]

# r1 = 916.442: this is the average total score for regular classes without an aide.
# r2 = 4.306: this represents the impact of an aide.
