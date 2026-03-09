# we need to load file from my Mac
load("~/Documents/R/data_needed/poe5rdata/cex5_small.rdata")

# we only retain households whose foodaway are greater than 0
plot_data <- subset(cex5_small, foodaway > 0)

install.packages("ggplot2")
library(ggplot2)

# scatter plot
ggplot(data = plot_data, aes(x = income, y = log(foodaway)))+
  geom_point(color = "steelblue", alpha = 0.4, size = 1)+
  geom_smooth(method = "lm", color = "black", linetype = 1)+
  labs(title = "Scatter plot of ln(FOODAWAY) vs. INCOME",
       subtitle = "with fitted regression line",
       x = "Monthly Income ($100s)",
       y = "ln(Foodaway Expenditure)")

# we can see an upward slope.
# as household income increases, the predicted log-expenditure on eating out also increases.