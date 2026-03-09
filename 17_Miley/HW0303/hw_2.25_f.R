# we need to load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/cex5_small.rdata")

x <- cex5_small$income
y <- log(cex5_small$foodaway)

# calculate y_hat
y_hat <- b1 + b2 * x
# calculate residuals
e_hat <- y - y_hat
sum(e_hat)
print(sum(e_hat))

install.packages("ggplot2")
library(ggplot2)

# create a scatter plot of residuals vs. income
ggplot(data = cex5_small, aes(x = income, y = e_hat))+
  geom_point(color = "steelblue", alpha = 0.5, size = 1)
  
# Explanation
# The residuals are randomly distributed and show no patterns.
# The log-linear model provides a reliable estimation of the relationship between income and food expenditure.
