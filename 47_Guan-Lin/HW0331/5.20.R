#(e)
set.seed(456)
sigma2 <- 1000
#N needs to be even
Ns <- c(100,500,1000,5000)
rep <- 5000
#N <- 5000

for (N in Ns){
  varb2 <- numeric(rep)
  varBeta2Mean <- numeric(rep)
  inv_sx2 <- numeric(rep)
  inv_diff <- numeric(rep)
  for (r in 1:rep){
    x <- runif(N,0,10)
    x <- sort(x)
    #(i)
    #var(b2|x)
    deno <- sum((x-mean(x))^2) 
    varb2[r] <- sigma2/deno
    
    #var(beat2,mean|x)
    x1 <- x[1:(N/2)]
    x2 <- x[((N/2)+1):N]
    xbar1 <- mean(x1)
    xbar2 <- mean(x2)
    deno2 <- N * (xbar2-xbar1)^2
    varBeta2Mean[r] <- (4 * sigma2) / deno2
    #cat("N =",N,"\n")
    #cat("x: ",x,"\n")
    #cat("var(b_2|x):",varb2,"\n")
    #cat("beta_2,mean:",varBeta2Mean,"\n\n")
    
    #(ii)
    sx2 <- mean((x-mean(x))^2)
    inv_sx2[r] <- 1 /sx2
    inv_diff[r] <- 4/(xbar2-xbar1)^2
  }
  cat("N =",N,"\n")
  
  cat("var(b2|x) ≈",mean(varb2),"\n")
  cat("var(beta2_mean|x) ≈",mean(varBeta2Mean),"\n")
  
  cat("E[(sx^2)^(-1)] ≈",mean(inv_sx2),"\n")
  cat("E[4/(xbar2-xbar1)^2] ≈",mean(inv_diff),"\n\n")
  
  }
