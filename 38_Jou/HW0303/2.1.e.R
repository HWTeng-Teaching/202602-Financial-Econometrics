x <- c(3,2,1,-1,0)
y <- c(4,2,3,1,0)

xbar <- 1
ybar <- 2

mod <- lm(y ~ x)

plot(x, y, pch=16, xlab="x", ylab="y",
     main="Scatter Plot with Fitted Line")
abline(mod, lwd=2)

points(xbar, ybar, pch=19, col="red", cex=1.4)
abline(v=xbar, h=ybar, lty=2, col="red")

