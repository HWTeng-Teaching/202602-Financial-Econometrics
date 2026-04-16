
rm(list=ls()) # Caution: this clears the Environment

alpha = 0.05
df = 60
qt(1-alpha/2,60)

#5.20--------------

#(e,i)

N = c(100,500,1000,5000)
sigma2 = 1000

for (n in N) {
  x = runif(n,0,10)
  x = sort(x)
  x1_bar = mean(x[1:(n/2)])
  x2_bar = mean(x[(n/2+1):n])
  x_bar = mean(x)
  var_b2 = sigma2/sum((x-x_bar)^2)
  var_B2 = 4*sigma2/n * (1/(x2_bar-x1_bar)^2)
  cat(n,'var(b2)=',var_b2,'var(B2)=',var_B2,"\n")
}

#(e,ii)

for (n in N) {
  sx2_inv_vec = numeric(100)
  dif_vec = numeric(100)
  for (i in 1:100){
    x = runif(n,0,10)
    x = sort(x)
    x1_bar = mean(x[1:(n/2)])
    x2_bar = mean(x[(n/2+1):n])
    x_bar = mean(x)
    
    sx2 = sum((x - x_bar)^2) / n
    sx2_inv_vec[i] = 1/sx2
    dif = 4/(x2_bar-x1_bar)^2
    dif_vec[i] = dif
  }
  cat(n,'=',mean(sx2_inv_vec),'=',mean(dif_vec),"\n")
}



