x = c(3, 1, 2, 0, -1)
y = c(4, 3, 2, 0, 1)
x_bar = mean(x)
y_bar = mean(y)
model = lm(y~x)

png(filename = "regression.png", width = 800, height = 600, res = 100)
plot(x, y, main = "linear regression in base R",
    xlab = "X", ylab = "Y",
    pch = 19, col = "blue", cex = 1.5,
    xlim = c(-1.5, 3.5), ylim = c(-0.5, 4.5))
abline(model, col = "red", lwd = 2)
text(x, y, labels = paste0("(", x, ", ", y, ")"), pos = 4, col ="blue")
points(x_bar, y_bar, pch = 4, col = "black", cex = 2, lwd =3)
text(1, 2, labels = "average(1, 2)", pos = 2, col = "black", font = 2)
dev.off()