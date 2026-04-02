#e
set.seed(123)
simulate=function(N){
  x <- runif(N, 0, 10)
  sigma2=1000
  e <- rnorm(N, 0, sqrt(sigma2))
  
  beta1= 1
  beta2= 2
  y=beta1+beta2*x+e
  
  model=lm(y ~ x)
  
  sx2=mean( (x - mean(x))^2 )   
  var_b2=sigma2/(N*sx2)
  
  x_sorted=sort(x)
  y_sorted=y[order(x)]
  x1_bar=mean(x_sorted[1:(N/2)])
  x2_bar=mean(x_sorted[(N/2+1):N])
  
  var_mean=(4*sigma2 / N) / (x2_bar - x1_bar)^2
 
  inv_sx2 = 1 / sx2
  inv_gap = 4 / (x2_bar - x1_bar)^2
  
  return(c(var_b2, var_mean, inv_sx2, inv_gap))
}

Ns=c(100, 500, 1000, 5000)

results=t(sapply(Ns, simulate))
colnames(results)= c("var_b2", "var_mean", "E[1/sx2]", "E[4/(gap^2)]")

results
                                           