# We need to load the file from my Mac
load("~/Documents/R/data_needed/poe5rdata/cex5_small.rdata")

install.packages("ggplot2")
library(ggplot2)

# construct a histogram of FOODAWAY
ggplot(cex5_small, aes(foodaway))+
  geom_histogram(bins = 10, color = "black", fill = "steelblue", alpha = 0.5)+
  labs(title = "Histogram of FOODAWAY",
       x = "Food away from home expenditure per month per person")

# FOODAWAY's summary statistics
summary(cex5_small["foodaway"])

# The result is as below:
# mean = 49.27
# median = 32.55
# 25th percentiles = 12.04
# 75th percentiles = 67.50

# the data is skewed to the right.
# most people spend a small amount, but a few households spend a lot.



