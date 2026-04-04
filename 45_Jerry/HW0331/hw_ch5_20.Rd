set.seed(123)

sigma2 <- 1000
sample_sizes <- c(100,500,1000,5000)

results <- list()

for (N in sample_sizes) {
	
  x <- runif(N, min = 0, max = 10)
  x_sorted <- sort(x)

  x1 <- x_sorted[1:N/2]
  x2 <- x_sorted[(N/2 + 1):N]
    
  xbar1 <- mean(x1)
  xbar2 <- mean(x2)
    
  sx2 <- mean((x - mean(x))^2)
    
  var_ols <- sigma2 / (N * sx2)
  diff <- xbar2 - xbar1
  var_mean <- 4 * sigma2 / (N * diff^2)

  inv_sx2 <- 1 / sx2
  inv_x2x1 <- 4 / (diff^2)
  
  cat("\nSample size N =", N, "\n")
  cat("var(b2|X)            =", var_ols, "\n")
  cat("var(beta_2mean|X)    =", var_mean, "\n")
  cat("E[(sx^2)^(-1)]       =", inv_sx2, "\n")
  cat("E[4/(xbar2-xbar1)^2] =", inv_x2x1, "\n")
}