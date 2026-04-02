data_url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata" 
load(url(data_url))

par(mfrow=c(1,3))
plot(collegetown$sqft, res_A, main="Log-Lin: Res vs SQFT", ylab="Residuals", xlab="SQFT")
abline(h=0, col="red")
plot(collegetown$sqft, res_B, main="Log-Log: Res vs SQFT", ylab="Residuals", xlab="SQFT")
abline(h=0, col="red")
plot(collegetown$sqft, res_C, main="Linear: Res vs SQFT", ylab="Residuals", xlab="SQFT")
abline(h=0, col="red")