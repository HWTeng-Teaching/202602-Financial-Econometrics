library(POE5Rdata)
data("collegetown")
library(stargazer)
install.packages("tseries")
library(tseries)

#4.25(a)
  mod_a = lm(log(price)~sqft, data = collegetown)
  stargazer(mod_a,type="text")
  b2_a = coef(mod_a)[2]
  xbar = mean(collegetown$sqft)
  ybar = mean(collegetown$price)
  slope_a = b2_a * ybar
  ela_a = b2_a * xbar
  
  table_a = data.frame(Item = c("slope","elasticity"),
                            Value = c(slope_a,ela_a))
  stargazer(table_a,type = "text",summary = FALSE)
  
#4.25(b)
  mod_b = lm(log(price)~log(sqft), data = collegetown)
  stargazer(mod_b,type="text")
  b2_b = coef(mod_b)[2]
  xbar_b = mean(collegetown$sqft)
  ybar_b = mean(collegetown$price)
  slope_b = b2_b * ybar_b / xbar_b
  ela_b = b2_b
  
  table_b = data.frame(Item = c("slope","elasticity"),
                       Value = c(slope_b,ela_b))
  stargazer(table_b,type = "text",summary = FALSE)
  
#4.25(b)
  mod_c = lm(price~sqft, data = collegetown)
  gen_r2_a = cor(collegetown$price,exp(fitted(mod_a)))^2
  gen_r2_b = cor(collegetown$price,exp(fitted(mod_b)))^2
  r2_c = summary(mod_c)$r.squared
  table_c = data.frame(Item = c("generalized R^2_a","generalized R^2_b","R^2_c"),
                       Value = c(gen_r2_a,gen_r2_b,r2_c))
  stargazer(table_c,type = "text",summary = FALSE)
  
#4.25(d)
  par(mfrow = c(1,3))
  hist(resid(mod_a),breaks = 20, main = "log-linear",xlab = "residuals",col="steelblue")
  hist(resid(mod_b),breaks = 20, main = "log-log",xlab = "residuals",col="steelblue")
  hist(resid(mod_c),breaks = 20, main = "linear",xlab = "residuals",col="steelblue")
  
  j_a = jarque.bera.test(resid(mod_a))
  j_b = jarque.bera.test(resid(mod_b))
  j_c = jarque.bera.test(resid(mod_c))
  table_d = data.frame(Model = c("residuals_a", "residuals_b", "residuals_c"),
                       X_squared = c(j_a$statistic, j_b$statistic, j_c$statistic),
                       df = c(j_a$parameter, j_b$parameter, j_c$parameter),
                       p_value = c(j_a$p.value, j_b$p.value, j_c$p.value))
  stargazer(table_d, type = "text", summary = FALSE,digits = 4,
            title = "Jarque-Bera Test Results")
  
#4.25(e)
  par(mfrow = c(1,3))
  plot(collegetown$sqft,resid(mod_a),pch =16,col = "steelblue",
       xlab = "SQFT",ylab = "Residuals",main = "log-linear")
  abline(h = 0,lwd = 2)
  plot(collegetown$sqft,resid(mod_b),pch =16,col = "steelblue",
       xlab = "SQFT",ylab = "Residuals",main = "log-log")
  abline(h = 0,lwd = 2)
  plot(collegetown$sqft,resid(mod_c),pch =16,col = "steelblue",
       xlab = "SQFT",ylab = "Residuals",main = "linear")
  abline(h = 0,lwd = 2)
  
#4.25(f)
  house_27 = data.frame(sqft = 27)
  pred_a = exp(predict(mod_a,newdata = house_27))
  pred_b = exp(predict(mod_b,newdata = house_27))
  pred_c = predict(mod_c,newdata = house_27)
  table_f = data.frame(Model = c("A","B","C"),
                      Predict_Value = c(pred_a,pred_b,pred_c))
  stargazer(table_f,type = "text",summary = FALSE)
  
#4.25(g)
  pi_a = exp(predict(mod_a,newdata = house_27,interval = "prediction",level = 0.95))
  pi_b = exp(predict(mod_b,newdata = house_27,interval = "prediction",level = 0.95))
  pi_c = predict(mod_c,newdata = house_27,interval = "prediction",level = 0.95)
  table_g = data.frame(Model = c("A", "B", "C"),
                       Fit =c(pi_a[1], pi_b[1], pi_c[1]),
                       Lower = c(pi_a[2], pi_b[2], pi_c[2]),
                       Upper = c(pi_a[3], pi_b[3], pi_c[3]))
  stargazer(table_g,type = "text", summary = FALSE)
            
#5.20(e)
  set.seed(123)
  sigma2 = 1000
  Ns = c(100,500,1000,5000)
  R = 1000  
  
  results <- list()
  
  for (N in Ns) {
    
    var_ols <- numeric(R)
    var_mean <- numeric(R)
    inv_sx2 <- numeric(R)
    inv_diff <- numeric(R)
    
    for (r in 1:R) {
      
      x <- runif(N, 0, 10)
      
      sx2 <- mean((x - mean(x))^2)
      var_ols[r] <- sigma2 / (N * sx2)
      
      x_sorted <- sort(x)
      x1 <- x_sorted[1:(N/2)]
      x2 <- x_sorted[(N/2 + 1):N]
      
      xbar1 <- mean(x1)
      xbar2 <- mean(x2)
      
      var_mean[r] <- (4 * sigma2 / N) / ((xbar2 - xbar1)^2)
      
      inv_sx2[r] <- 1 / sx2
      inv_diff[r] <- 4 / (xbar2 - xbar1)^2
    }
    
    results[[as.character(N)]] <- c(
      mean(var_ols),
      mean(var_mean),
      mean(inv_sx2),
      mean(inv_diff)
    )
  }
  table_e2 = data.frame(N = c("100", "500", "1000","5000"),
                       var_b2 = c(results$`100`[1], results$`100`[2], results$`100`[3],results$`100`[4]),
                       var_b2_hat_mean = c(results$`500`[1], results$`500`[2], results$`500`[3],results$`500`[4]),
                       ex_1_sx2 = c(results$`1000`[1], results$`1000`[2], results$`1000`[3],results$`1000`[4]),
                       ex_var = c(results$`5000`[1], results$`5000`[2], results$`5000`[3],results$`5000`[4]))
  stargazer(table_e2,type = "text", summary = FALSE)
  
  
