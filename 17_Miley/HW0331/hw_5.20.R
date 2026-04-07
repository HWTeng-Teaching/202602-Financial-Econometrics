# part e
N_list <- c(100, 500, 1000, 5000)

for(N in N_list){
  # generate N random variables in order
  x <- sort(runif(N, 0, 10))
  # calculate mean
  mean_x <- mean(x)
  mean_x1 <- mean(x[1:(N/2)])
  mean_x2 <- mean(x[((N/2)+1):N])
  # OLS variance var(b2|x) = sigma^2 / sum((xi-x_bar)^2)
  var_b2 <- 1000/sum((x-mean_x)^2)
  # var(B2_hat,mean|x) = 4*sigma^2 / N*(x2_bar-x1_bar)^2
  var_b2_hat <- (4*1000)/(N*(mean_x2-mean_x1)^2)
  # (sx^2)^(-1)
  sx <- sum((x-mean_x)^2)/N
  sx_1 <- 1/sx
  sx_2 <- 4/((mean_x2-mean_x1)^2)
  cat("When the sample size is: ", N, "\n")
  cat("mean of x1 and x2 are: ", mean_x1, " and", mean_x2,"\n")
  cat("OLS variance = ", var_b2, "\n")
  cat("professor's variance = ", var_b2_hat, "\n")
  cat("(sx^2)^(-1) = ", sx_1, "\n")
  cat("4/(x2_bar-x1_bar)^2 = ", sx_2, "\n")
}

# part e
N_list <- c(100, 500, 1000, 5000)

for(N in N_list){
  # generate N random variables 
  x <- runif(N, 0, 10)
  # calculate mean
  mean_x <- mean(x)
  mean_x1 <- mean(x[1:(N/2)])
  mean_x2 <- mean(x[((N/2)+1):N])
  # OLS variance var(b2|x) = sigma^2 / sum((xi-x_bar)^2)
  var_b2 <- 1000/sum((x-mean_x)^2)
  # var(B2_hat,mean|x) = 4*sigma^2 / N*(x2_bar-x1_bar)^2
  var_b2_hat <- (4*1000)/(N*(mean_x2-mean_x1)^2)
  # (sx^2)^(-1)
  sx <- sum((x-mean_x)^2)/N
  sx_1 <- 1/sx
  sx_2 <- 4/((mean_x2-mean_x1)^2)
  cat("When the sample size is: ", N, "\n")
  cat("mean of x1 and x2 are: ", mean_x1, " and", mean_x2,"\n")
  cat("OLS variance = ", var_b2, "\n")
  cat("professor's variance = ", var_b2_hat, "\n")
  cat("(sx^2)^(-1) = ", sx_1, "\n")
  cat("4/(x2_bar-x1_bar)^2 = ", sx_2, "\n")
}

