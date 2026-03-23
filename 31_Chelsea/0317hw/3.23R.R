data(package='POE5Rdata')
library('POE5Rdata')
data('collegetown',package='POE5Rdata')
model<- lm(price~I(sqft^2),data=collegetown)
summary(model)
a2_hat<-coef(model)['I(sqft^2)']
se_a2<-summary(model)$coefficients['I(sqft^2)','Std. Error']
#ME=dY/dX=2*a2*sqft(marginal effect at sqft=20)
ME<-2* a2_hat*20
#H0:40*a2<=13; H1:40*a2>13
t_stat<-(ME-13)/(40*se_a2) #ME=40*a2 所以se(ME)=40*se(a2)
#右尾檢定 reject region= t*>t(df of SSE,5%)
p_value=1-pt(t_stat,df=df.residual(model))
cat('邊際效應:',ME, '\nt統計量',t_stat, '\n P值',p_value)

#3.23(b)
a2_hat<-coef(model)['I(sqft^2)']
se_a2<-summary(model)$coefficients['I(sqft^2)','Std. Error']
#ME=dY/dX=2*a2*sqft(marginal effect at sqft=40)
ME<-2* a2_hat*40
#H0:80*a2<=13; H1:80*a2>13
t_stat<-(ME-13)/(80*se_a2) #ME=80*a2 所以se(ME)=80*se(a2)
#右尾檢定 reject region= t*>t(df of SSE,5%)
p_value=1-pt(t_stat,df=df.residual(model))
cat('邊際效應:',ME, '\nt統計量',t_stat, '\n P值',p_value)

#3.23(c)
new_house <- data.frame(sqft = 20)
pred_c <- predict(model, new_house, interval = "confidence", level = 0.95)
print(pred_c)

#3.23(d)
# 1. 抓出所有 sqft 剛好等於 20 的房子
houses_20 <- subset(collegetown, sqft == 20)
# 2. 計算這些房子的平均售價
sample_mean_20 <- mean(houses_20$price)
# 3. 印出來對照一下
cat('樣本中 2000 呎房子的平均售價:', sample_mean_20)
