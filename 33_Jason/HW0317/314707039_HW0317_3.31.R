#已將tuna.rdata匯入Environment
#a小題
summary(tuna$sal1)
summary(tuna$apr1)
sd_sal1<-sd(tuna$sal1)
sd_apr1<-sd(tuna$apr1)
mean_sal1<-mean(tuna$sal1)
mean_apr1<-mean(tuna$apr1)
CV_sal1<-sd_sal1/mean_sal1
CV_apr1<-sd_apr1/mean_apr1
cat("SAL1的標準差為:",sd_sal1,"\n")
cat("APR1的標準差為:",sd_apr1,"\n")
cat(" sales的變異係數(CV):", CV_sal1, "\n")
cat(" price的變異係數(CV):", CV_apr1, "\n")

tuna$week<-1:nrow(tuna) 
par(mfrow = c(2, 1)) # 先分割畫面
plot(tuna$week, tuna$sal1, type = "o", 
     main = "SAL1 Weekly Trend",
     xlab="Week", ylab="Sales")
plot(tuna$week, tuna$apr1, type = "o",
     main = "APR1 Weekly Trend",
     xlab="Week", ylab="Sales")

#b小題
par(mfrow=c(1,1)) # 恢復單一畫面
plot(tuna$apr1, tuna$sal1, 
     xlab = "Price per can (APR1)", 
     ylab = "Unit Sales (SAL1)",
     main = "Sales vs. Price",
     pch = 19, col = "red")

#c小題
#price1(單位：美分)
tuna$price1<-100*tuna$apr1
model_tuna<-lm(sal1~price1,data = tuna)
smodel_tuna<-summary(model_tuna)
b2_hat<-coef(model_tuna)[[2]]
se_b2<-coef(smodel_tuna)[2,2]
t_crit<-qt(0.975,df=df.residual(model_tuna))
lower_bound<-b2_hat-t_crit*se_b2
upper_bound<-b2_hat+t_crit*se_b2
cat("價格每增加1美分，平均銷售額變動:",b2_hat,"\n")
cat("99% Confidence Interval:[",lower_bound, ",", upper_bound,"]\n")

#d小題 
new_test<- data.frame(price1=70)
pred_d<- predict(model_tuna,newdata =new_test,interval="confidence",level=0.90)
cat("預期銷量 (fit):", pred_d[1, "fit"], "\n")
cat("90%信賴區間:[",pred_d[1,"lwr"],",", pred_d[1,"upr"],"]\n")

#e小題
mean_price<-mean(tuna$price1)
elas_hat<-b2_hat*(mean_price/mean_sal1)
ci_b2<-confint(model_tuna,"price1",level = 0.95)
ci_elas<-ci_b2*(mean_price/mean_sal1)
cat("彈性點估計:",elas_hat,"\n")
cat("彈性95%信心區間:[",ci_elas[1],",",ci_elas[2],"]\n")

#f小題
#H0:elasticity=3 vs H1:elasticity !=3
null_elas<- -3
se_elas<-se_b2*(mean_price/mean_sal1)
t_stat_elas<-((elas_hat-null_elas)/se_elas)
p_value_elas<-2*pt(abs(t_stat_elas),df.residual(model_tuna),lower.tail = FALSE)
t_crit_elas<-qt(0.95,df=df.residual(model_tuna))
cat("t統計量:",t_stat_elas,"\n")
cat("p-value:",p_value_elas,"\n")
cat("拒絕域:t>",t_crit_elas,"or t<-",t_crit_elas,"\n")




