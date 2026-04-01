set.seed(123)

simulate <- function(N, beta1=1, beta2=2, sigma2=1000){
  
  x <- runif(N, 0, 10)
  e <- rnorm(N, 0, sqrt(sigma2))
  y <- beta1 + beta2 * x + e
  
  # 排序
  order_index <- order(x)
  x <- x[order_index]
  y <- y[order_index]
  
  # 分組
  half <- N/2
  x1 <- x[1:half]
  x2 <- x[(half+1):N]
  y1 <- y[1:half]
  y2 <- y[(half+1):N]
  
  # mean estimator
  beta_mean <- (mean(y2) - mean(y1)) / (mean(x2) - mean(x1))
  
  # OLS
  model <- lm(y ~ x)
  beta_ols <- coef(model)[2]
  
  # variance (given x)
  var_ols <- sigma2 / sum( (x - mean(x))^2 )
  var_mean <- 4 * sigma2 / ( N * (mean(x2)-mean(x1))^2 )
  
  # s_x^2
  sx2 <- mean( (x - mean(x))^2 )
  
  return(list(
    var_ols = var_ols,
    var_mean = var_mean,
    inv_sx2 = 1/sx2,
    term_mean = 4/(mean(x2)-mean(x1))^2
  ))
}

Ns <- c(100, 500, 1000, 5000)

results <- lapply(Ns, function(N){
  replicate(500, simulate(N), simplify = FALSE)
})

summarize <- function(res){
  var_ols <- mean(sapply(res, function(x) x$var_ols))
  var_mean <- mean(sapply(res, function(x) x$var_mean))
  inv_sx2 <- mean(sapply(res, function(x) x$inv_sx2))
  term_mean <- mean(sapply(res, function(x) x$term_mean))
  
  return(c(var_ols, var_mean, inv_sx2, term_mean))
}

output <- t(sapply(results, summarize))
colnames(output) <- c("var(b2|x)", "var(betahat2,mean|x)", "E[(sx2)^(-1)]", "E[4/(x2-x1)^2]")
rownames(output) <- Ns

print(output)