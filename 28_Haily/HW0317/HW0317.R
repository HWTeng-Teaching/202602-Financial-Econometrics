data("collegetown")

#3.23(a)
  alpha = 0.05
  mod1 = lm(price ~ I(sqft^2) , data = collegetown)
  a2 = coef(mod1)[2]
  ME = 2*a2*20
  se_a2 = summary(mod1)$coefficients[2,2]
  se_ME = 40*se_a2
  df = df.residual(mod1)
  
  #cal t-stat
  t0 = (ME-13)/se_ME
  
  #RR
  tc = qt(1-alpha,df)
  
  #P-Value
  p_value = pt(t0,df,lower.tail = FALSE)
  
  result_table = data.frame(Item = c("t-statistic","Critical value","P-value"),
                            Value = c(t0,tc,p_value))
  stargazer(result_table,type = "text",summary = FALSE)

#3.23(b)
  ME_b = 2*a2*40
  se_ME_b = 80*se_a2
  
  #cal t-stat
  t0_b = (ME_b-13)/se_ME_b
  
  #P-Value
  p_value_b = pt(t0_b,df,lower.tail = FALSE)
  
  result_table_b = data.frame(Item = c("t-statistic","Critical value","P-value"),
                            Value = c(t0_b,tc,p_value_b))
  stargazer(result_table_b,type = "text",summary = FALSE)
 
#3.23(c)
  price_hat = coef(mod1)[1] + coef(mod1)[2]*20^2

  #Var
  vcov = vcov(mod1)
  var_a1 = vcov[1,1]
  var_a2 = vcov[2,2]
  cov12 = vcov[2,1]
  var_price = var_a1 + 400^2*var_a2 + 2*400*cov12
  
  #se
  se_price = sqrt(var_price)
  
  #critical value
  tc_c = qt(1-alpha/2,df)
  
  #C.I.
  lowerB = price_hat - tc_c*se_price
  upperB = price_hat + tc_c*se_price
  
  result_table_c = data.frame(Interval = c("LowerBound","UpperBound"),
                              Value = c(lowerB,upperB))
  stargazer(result_table_c,type = "text",summary = FALSE)
  
#3.23(d)
  subset_d = subset(collegetown,sqft == 20)
  mean_price = mean(subset_d$price)
  
  result_table_d = data.frame(Mean = c("price"),
                              Value = c(mean_price))
  stargazer(result_table_d,type = "text",summary = FALSE)
  
  
#3.31(a)
  data("tuna")
  
  stargazer(tuna[,c("sal1","apr1")],
            type="text",
            summary.stat=c("mean","min","max","sd" ))
  
  #plot
  plot(tuna$sal1,type = "o",pch = 19,
       xlab = "Week",ylab = "Sales(SAL1)",
       main = "Weekly sales of Tuna")
  plot(tuna$apr1,type = "o",pch = 19,
       xlab = "Week",ylab = "Price(APR1)",
       main = "Weekly price of Tuna")
  
#3.23(b)
  plot(tuna$sal1,tuna$apr1,pch =16,col = "steelblue",
       xlab = "Sales(SAL1)",ylab = "Price(APR1)",
       main = "Sales vs Price")
  abline(lm(apr1 ~ sal1,data = tuna),col = "indianred",lwd = 2)

#3.23(c)
  tuna$price1 = 100*tuna$apr1
  #point estimate
  mod2 = lm(sal1 ~ price1,data = tuna)
  stargazer(mod2,type="text")
  
  #C.I.
  b2 = coef(mod2)[2]
  se_b2 = summary(mod2)$coefficients[2,2]
  df2 = df.residual(mod2)
  tc_c2 = qt(1-alpha/2,df2)
  lowerB2 = b2 - tc_c2*se_b2
  upperB2 = b2 + tc_c2*se_b2
  
  result_table_c2 = data.frame(Interval = c("LowerBound","UpperBound"),
                              Value = c(lowerB2,upperB2))
  stargazer(result_table_c2,type = "text",summary = FALSE)
  
#3.23(d)
  alpha2 = 0.1
  sal1_hat = coef(mod2)[1] + coef(mod2)[2]*70
  
  #Var
  vcov = vcov(mod2)
  var_b1 = vcov[1,1]
  var_b2 = vcov[2,2]
  cov_12 = vcov[2,1]
  var_sal1 = var_b1 + 70^2*var_b2 + 2*70*cov_12
  
  #se
  se_sal1 = sqrt(var_sal1)
  
  #critical value
  tc_d = qt(1-alpha2/2,df)
  
  #C.I.
  lowerB3 = sal1_hat - tc_d*se_sal1
  upperB3 = sal1_hat + tc_d*se_sal1
  
  result_table_d2 = data.frame(Interval = c("LowerBound","UpperBound"),
                              Value = c(lowerB3,upperB3))
  stargazer(result_table_d2,type = "text",summary = FALSE)
  
#2.23(e)
  xbar = mean(tuna$price1)
  ybar = mean(tuna$sal1)
  
  #elasticity
  e_hat = b2*xbar/ybar
  
  result_e = data.frame(Type = c("price elasticity"),
                        Value = c(e_hat))
  stargazer(result_e,type = "text",summary = FALSE)
  
  se_e = se_b2*xbar/ybar
  
  lowerB4 = e_hat - tc_c*se_e
  upperB4 = e_hat + tc_c*se_e
  
  result_table_e = data.frame(Interval = c("LowerBound","UpperBound"),
                               Value = c(lowerB4,upperB4))
  stargazer(result_table_e,type = "text",summary = FALSE)
  
#3.23(f)
  #cal t-stat
  t0_f = (e_hat-(-3))/se_e
  
  #RR
  tc_f = qt(1-alpha2/2,df2)
  
  #p-value
  p_value_f = 2*pt(-abs(t0_f),df2)
  
  result_table_f = data.frame(Item = c("t-statistic","Critical value","P-value"),
                              Value = c(t0_f,tc_f,p_value_f))
  stargazer(result_table_f,type = "text",summary = FALSE)
  
  