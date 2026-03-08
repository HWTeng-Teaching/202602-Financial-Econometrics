# we need to load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/cex5_small.rdata")

# create a new data set (households including a member with an advanced degree)
advanced_data <- subset(cex5_small, advanced == 1)
summary(advanced_data["foodaway"])

# The result is as below:
# mean = 73.15
# median = 48.15

# create another data set (households including a member with a college degree)
college_data <- subset(cex5_small, college == 1)
summary(college_data["foodaway"])

# The result is as below:
# mean = 48.60
# median = 36.11

# create another data set (households with no advanced or college degree memver)
no_data <- subset(cex5_small, advanced == 0 & college == 0)
summary(no_data["foodaway"])

# The result is as below:
# mean = 39.01
# median = 26.02


