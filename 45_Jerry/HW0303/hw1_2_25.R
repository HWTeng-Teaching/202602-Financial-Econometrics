library(POE5Rdata)
data("cex5_small")
?cex5_small
hist(cex5_small$foodaway, col='grey')
summary(cex5_small$foodaway)

summary(cex5_small$foodaway[cex5_small$college==1])
summary(cex5_small$foodaway[cex5_small$advanced==1])
summary(cex5_small$foodaway[cex5_small$college==0 & cex5_small$advanced==0])
table(cex5_small$college, cex5_small$advanced)

hist(log(cex5_small$foodaway), col='grey')
summary(log(cex5_small$foodaway))

mod1 <- lm(log(foodaway) ~ income, data = cex5_small[cex5_small$foodaway > 0, ])
smod1 <- summary(mod1)
smod1

plot(cex5_small$income[cex5_small$foodaway > 0],
     log(cex5_small$foodaway[cex5_small$foodaway > 0]),
     col="grey")

abline(mod1, col="blue", lwd=2)

resids <- residuals(mod1)
plot(cex5_small$income[cex5_small$foodaway > 0],
	 resids,
	 col="grey")
