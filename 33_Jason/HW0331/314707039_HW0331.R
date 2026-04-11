set.seed(122)#設定固定隨機種子
sigma2<-1000
N_values<-c(100,500,1000,5000)
# 建立儲存結果的資料框
results <- data.frame(N = N_values, 
var_b2_ols = NA,var_b2_mean = NA, 
estimated_sx2_inv = NA, estimated_term2 = NA)

for(i in 1:length(N_values))
{
  N<-N_values[i]
  x<-runif(N,0,10)
  x_sorted<-sort(x) # 依照題目要求進行排序
  
  half <- N / 2 # 分割成前半部與後半部
  x1_bar <- mean(x_sorted[1:half])
  x2_bar <- mean(x_sorted[(half+1):N])
  #e小題
  #i.
  var_ols <- sigma2/sum((x - mean(x))^2)
  var_mean <- (4*sigma2)/(N*(x2_bar-x1_bar)^2)
  #ii.
  sx2 <- sum((x-mean(x))^2)/N
  term_i<-1/sx2
  term_ii = 4/(x2_bar - x1_bar)^2
  results$var_b2_ols[i]  <- var_ols
  results$var_b2_mean[i] <- var_mean
  results$estimated_sx2_inv[i]   <- term_i
  results$estimated_term2[i]     <- term_ii
}
print(results)

