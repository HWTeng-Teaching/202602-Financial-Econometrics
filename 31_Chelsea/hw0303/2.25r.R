data(package="POE5Rdata")
library(POE5Rdata)
data("cex5_small", package="POE5Rdata")
hist(cex5_small$foodaway)
summary(cex5_small$foodaway)

mean(cex5_small$foodaway, na.rm=TRUE)

median(cex5_small$foodaway, na.rm=TRUE)
quantile(cex5_small$foodaway, probs=c(0.25,0.75), na.rm=TRUE)

names(cex5_small)
#advanced degree
mean(cex5_small$foodaway[cex5_small$advanced==1], na.rm=TRUE)
median(cex5_small$foodaway[cex5_small$advanced==1], na.rm=TRUE)
#college degree
mean(cex5_small$foodaway[cex5_small$college==1], na.rm=TRUE)
median(cex5_small$foodaway[cex5_small$college==1], na.rm=TRUE)
#no degree
mean(cex5_small$foodaway[cex5_small$advanced==0 &
                           cex5_small$college==0], na.rm=TRUE)
median(cex5_small$foodaway[cex5_small$advanced==0 &
                             cex5_small$college==0], na.rm=TRUE)
#2.25c
ln_food <- log(cex5_small$foodaway)
hist(ln_food)
summary(ln_food)
#2.25d
model_d <- lm(log(foodaway) ~ income, data = cex5_small, subset = foodaway > 0)
summary(model_d)
#2.25e
plot(log(foodaway) ~ income, data = cex5_small, subset = foodaway > 0, 
     main="ln(FOODAWAY) vs INCOME", pch=19, col="gray")
abline(model_d, col="red", lwd=2)
#2.25f
cex5_small_sub <- subset(cex5_small, foodaway > 0)
res <- resid(model_d)
plot(cex5_small_sub$income, res, main="Residuals vs Income", xlab="Income", ylab="Residuals")
abline(h = 0, col="blue", lty=2)
