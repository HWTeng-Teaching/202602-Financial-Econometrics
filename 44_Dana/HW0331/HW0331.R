set.seed(123)
N_values <- c(100, 500, 1000, 5000)
sigma2 <- 1000

for (N in N_values) {
  # Generate x from U(0,10)
  x <- runif(N, 0, 10)
  
  # Calculate traditional OLS variance part (i)
  var_b2_ols <- sigma2 / sum((x - mean(x))^2)
  
  # Sort x and calculate means for the two halves
  x_sorted <- sort(x)
  x1_bar <- mean(x_sorted[1:(N/2)])
  x2_bar <- mean(x_sorted[(N/2 + 1):N])
  
  # Calculate var of beta2_mean part (i)
  var_b2_mean <- (4 * sigma2) / (N * (x2_bar - x1_bar)^2)
  
  # Calculate expressions for part (ii)
  s2_x <- var(x) * (N - 1) / N # Sample variance using N divisor
  exp1 <- 1 / s2_x
  exp2 <- 4 / (x2_bar - x1_bar)^2
  
  cat("N =", N, "\n")
  cat("  var(b2_OLS) =", var_b2_ols, "| var(b2_mean) =", var_b2_mean, "\n")
  cat("  1/s^2_x =", exp1, "| 4/(x2_bar - x1_bar)^2 =", exp2, "\n\n")
}