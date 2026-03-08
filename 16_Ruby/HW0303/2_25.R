library(POE5Rdata)
data("star5_small") 

#(a)
# Histogram
hist(cex5_small$foodaway,
     main = "Histogram of FOODAWAY",
     xlab = "Food Away From Home Expenditure",
     col = "lightblue",
     breaks = 20)

# summary statistics
summary(cex5_small$foodaway)

# mean
mean(cex5_small$foodaway, na.rm = TRUE)

# median
median(cex5_small$foodaway, na.rm = TRUE)

# 25th and 75th percentiles
quantile(cex5_small$foodaway, probs = c(0.25,0.75), na.rm = TRUE)

#(b)
# advanced degree households
mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm=TRUE)
median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm=TRUE)

# college degree households
mean(cex5_small$foodaway[cex5_small$college == 1], na.rm=TRUE)
median(cex5_small$foodaway[cex5_small$college == 1], na.rm=TRUE)

# no college or advanced degree
mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm=TRUE)
median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm=TRUE)

#(c)
# 建立 log variable
cex5_small$lnfoodaway <- log(cex5_small$foodaway)

# histogram
hist(cex5_small$lnfoodaway,
     main="Histogram of ln(FOODAWAY)",
     xlab="log(FOODAWAY)",
     col="lightgreen",
     breaks=20)

# summary statistics
summary(cex5_small$lnfoodaway)

#(d)
model <- lm(log(foodaway) ~ income, data = cex5_small, subset = foodaway > 0)
summary(model)

#(e)
plot(cex5_small$income, cex5_small$lnfoodaway, xlab="INCOME", ylab="ln(FOODAWAY)",main="ln(FOODAWAY) vs INCOME", pch=16)
abline(model, col="red", lwd=2)

#(f)
model <- lm(log(foodaway) ~ income, data = cex5_small, subset = foodaway > 0)
plot(model$model$income, resid(model), xlab="INCOME", ylab="Residuals", main="Residuals vs INCOME", pch=16)
abline(h=0, col="red")