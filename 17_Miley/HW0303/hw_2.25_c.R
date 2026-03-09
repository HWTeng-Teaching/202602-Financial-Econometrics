# we need to load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/cex5_small.rdata")

# create a new column for ln(FOODAWAY)
log_foodaway <- log(cex5_small$foodaway)

install.packages("ggplot2")
library(ggplot2)

# construct a histogram of ln(FOODAWAY)
ggplot(cex5_small, aes(log_foodaway))+
  geom_histogram(bins = 10, color = "black", fill = "steelblue", alpha = 0.5)+
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "ln(FOODAWAY)")
  
# ln(FOODAWAY)'s summary statistics
summary(log(cex5_small$foodaway))

# The result is as below:
# mean = -inf
# median = 3.483
# 25th percentiles = 2.488
# 75th percentiles = 4.212

# Conclusion
# The number of observations for ln(FOODAWAY) is smaller than that of FOODAWAY.
# Because the original data set contains households with 0 expenditure on foodaway.
# Mathematically, the natural log of zero is undefined and approaches negative infinity.