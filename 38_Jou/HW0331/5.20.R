set.seed(123)

N_list <- c(100, 500, 1000, 5000)
R <- 5000  
sigma2 <- 1000

simulate <- function(N, sigma2 = 1000, R = 5000) {
  
  var_b2_ols <- numeric(R)
  var_b2_mean <- numeric(R)
  inv_sx2 <- numeric(R)
  term_mean <- numeric(R)
  
  for (r in 1:R) {
    
    x <- runif(N, min = 0, max = 10)
    
    x_sorted <- sort(x) 
    
    # Split into first half and second half
    half <- N/2
    x1 <- x_sorted[1:half]
    x2 <- x_sorted[(half + 1):N]
    
    # Means of the two halves
    xbar1 <- mean(x1)
    xbar2 <- mean(x2)
    
    # Sample variance with N
    sx2 <- mean((x - mean(x))^2)
    
    # var(b2 | x) for OLS
    # sum (xi - xbar)^2 = N * sx2
    var_b2_ols[r] <- sigma2 / sum((x - mean(x))^2)
    
    # var(b2_hat, mean | x)
    var_b2_mean[r] <- 4 * sigma2 / (N * (xbar2 - xbar1)^2)
    
    # Quantities in part (ii)
    inv_sx2[r] <- 1 / sx2
    term_mean[r] <- 4 / (xbar2 - xbar1)^2
  }
  
  data.frame(
    N = N,
    Estimate_var_b2 = mean(var_b2_ols),
    Estimate_var_b2_mean = mean(var_b2_mean),
    Estimate_inv_sx2 = mean(inv_sx2),
    Estimate_term_mean = mean(term_mean)
  )
}

results <- do.call(
  rbind,
  lapply(N_list, simulate, sigma2 = sigma2, R = R)
)

print(results)
