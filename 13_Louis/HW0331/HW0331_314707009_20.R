rm(list=ls())

library(POE5Rdata)

#(e)(f)
set.seed(123)


sigma2 <- 1000
N_list <- c(100, 500, 1000, 5000)


results <- data.frame(
  N = integer(),
  var_b2_given_x = numeric(),
  var_b2mean_given_x = numeric(),
  inv_sx2 = numeric(),
  four_over_gap2 = numeric()
)

for (N in N_list) {
  
  n_half <- N / 2
  
 
  x <- runif(N, min = 0, max = 10)
  
  
  xbar <- mean(x)
  sx2 <- mean((x - xbar)^2)
  
  # Var(b2 | x)
  var_b2 <- sigma2 / sum((x - xbar)^2)
  
  # 排序後分前後兩半
  x_sorted <- sort(x)
  xbar1 <- mean(x_sorted[1:n_half])
  xbar2 <- mean(x_sorted[(n_half + 1):N])
  
  gap <- xbar2 - xbar1
  
  # Var(beta2_mean | x)
  var_b2mean <- 4 * sigma2 / (N * gap^2)
  

  results <- rbind(
    results,
    data.frame(
      N = N,
      var_b2_given_x = var_b2,
      var_b2mean_given_x = var_b2mean,
      inv_sx2 = 1 / sx2,
      four_over_gap2 = 4 / (gap^2)
    )
  )
}

print(results)
