collegetown <- read.csv("http://www.principlesofeconometrics.com/poe5/data/csv/collegetown.csv")
#a小題
model<-lm(price~I(sqft^2), data = collegetown)
smodel<-summary(model)
#斜率微分後變為2*a2*SOFT，將SOFT=20代入，本題假設檢定為 H0:a2<=13/40,H1:a2>13/40
a2_hat<-coef(model)[[2]]
se_a2<-coef(smodel)[2,2] 
df = df.residual(model)
#計算統計量
t_stat_a<-(a2_hat-13/40)/(se_a2)
p_value_a<-pt(t_stat_a,df = df,lower.tail = FALSE) 
RR_a<-qt(0.05,498,lower.tail = FALSE)
cat("t統計量為:",t_stat_a,"\n")
cat("拒絕域為:t>",RR_a,"\n")
cat("p_value為:",p_value_a,"\n")

#b小題
#將SOFT=40代入，本題假設檢定為 H0:a2<=13/80,H1:a2>13/80
t_stat_b<-(a2_hat-13/80)/(se_a2)
p_value_b<-pt(t_stat_b,df = df,lower.tail = FALSE) 
RR_b<-qt(0.05,498,lower.tail = FALSE)
cat("t統計量為:",t_stat_b,"\n")
cat("拒絕域為:t>",RR_b,"\n")
cat("p_value為:",p_value_b,"\n")

#c小題
a1_hat<-coef(model)[[1]]
se_a1<-coef(smodel)[1,2]
cov=vcov(model)[1,2]
x0=20
y_hat<-a1_hat+a2_hat*(x0^2)
var_y_hat=(se_a1^2)+(x0^4)*(se_a2^2)+2*(x0^2)*cov
se_y_hat=sqrt(var_y_hat)
t=qt(0.975,df)
lower_bound_y_hat = y_hat - t*se_y_hat 
upper_bound_y_hat = y_hat + t*se_y_hat
cat("99% Confidence Interval: [",lower_bound_y_hat, ",", upper_bound_y_hat, "]\n")

#d小題
price_2000=collegetown$price[collegetown$sqft==20] 
sample_mean_2000 <- mean(price_2000) 
cat("2000 sqft真實平均房價:", sample_mean_2000, "\n") 





