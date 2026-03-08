x=c(3,2,1,-1,0)
y=c(4,2,3,1,0)

x_bar=mean(x)
y_bar=mean(y)

plot(x, y, pch=16, col="blue", main="Data and Fitted Regression Line",
     xlab="x", ylab="y")

b1 <- 1.2
b2 <- 0.8
abline(a=b1, b=b2, col="red", lwd=2)

points(x, y_hat, pch=17, col="orange") 

points(x_bar, y_bar, pch=19, col="green", cex=1.5)
