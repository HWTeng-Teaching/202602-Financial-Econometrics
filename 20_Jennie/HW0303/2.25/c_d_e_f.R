load("D:/碩一下/計量經濟/作業/HW1/25/cex5_small.rdata")
#c
cex5_small$ln_foodaway=log(cex5_small$foodaway)
ln_foodaway_positive=subset(cex5_small, foodaway > 0)

hist(ln_foodaway_positive$ln_foodaway,
     main="Histogram of ln(FOODAWAY)",
     xlab="ln(FOODAWAY)",
     col="blue",
     breaks=20)

summary(ln_foodaway_positive$ln_foodaway)
#d
reg=lm(ln_foodaway~income, data=ln_foodaway_positive)
summary(reg)
#e
plot(ln_foodaway_positive$income,
     ln_foodaway_positive$ln_foodaway,
     main="ln(FOODAWAY) vs INCOME",
     xlab="Household Income ($100 units)",
     ylab="ln(FOODAWAY)",
     pch=16,   
     col="blue")
abline(reg, col="red", lwd=2)
#f
residuals=reg$residuals
plot(ln_foodaway_positive$income,
     residuals,
     main="Residuals vs INCOME",
     xlab="Household Income ($100 units)",
     ylab="Residuals",
     pch=16,
     col="blue")
