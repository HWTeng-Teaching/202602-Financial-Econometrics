data(package='POE5Rdata')
library(POE5Rdata)
data('collegetown',package='POE5Rdata')
#ModelA為log-linear
modelA<-lm(log(price)~sqft,data=collegetown)
summary(modelA)
b2_A<-coef(modelA)['sqft'] 
elas<-b2_A*mean(collegetown$sqft)
#ModelB為log-log
modelB<-lm(log(price)~log(sqft),data=collegetown)
summary(modelB)
a2_B<-coef(modelB)['log(sqft)']
#modelC是linear
modelC<-lm(price~sqft,data=collegetown)
summary(modelC)
sum_C<-summary(modelC)
R2_C<-sum_C$r.squared
R2_B<-summary(modelB)$r.squared
#檢查殘差
resA<-residuals(modelA)
hist(resA,breaks=20, col='blue', main='histogram of As residuals',xlab='residuals',prob=TRUE)
resB<-residuals(modelB)
hist(resB,breaks=20, col='red', main='histogram of Bs residuals',xlab='residuals',prob=TRUE)
resC<-residuals(modelC)
hist(resC,breaks=20, col='green', main='histogram of Cs residuals',xlab='residuals',prob=TRUE)
#殘差常態分配檢定
install.packages('tseries')
library(tseries)
jb_A<-jarque.bera.test(resid(modelA))
jb_A
jb_B<-jarque.bera.test(resid(modelB))
jb_B
jb_C<-jarque.bera.test(resid(modelC))
jb_C
#殘差圖
par(mfrow=c(1,3))
plot(collegetown$sqft,resid(modelA),main='residual of A',col='blue',xlab='sqft',ylab='e')
abline(h=0,col='pink')
plot(collegetown$sqft,resid(modelB),main='residual of B',col='red',xlab='sqft',ylab='e')
abline(h=0,col='pink')
plot(collegetown$sqft,resid(modelC),main='residual of C',col='green',xlab='sqft',ylab='e')
abline(h=0,col='pink')
par(mfrow=c(1,1))
#2700sqft, sqft=27
#prediction value
new_data<-data.frame(sqft=27)
predict_logA<-predict(modelA,new_data)
predict_priceA<-exp(predict_logA)
predict_logB<-predict(modelB,new_data)
predict_priceB<-exp(predict_logB)
predict_priceC<-predict(modelC,new_data)
cat('模型A預測的房價:',predict_priceA,'\n模型B預測房價:',predict_priceB,'\n模型C預測房價:',predict_priceC)
#prediction interval
preint_logA<-predict(modelA,new_data,interval='prediction',level=0.95)
preint_priceA<-exp(preint_logA)
preint_logB<-predict(modelB,new_data,interval='prediction',level=0.95)
preint_priceB<-exp(preint_logB)
preint_priceC<-predict(modelC,new_data,interval='prediction',level=0.95)
cat('模型A預測的房價區間:',preint_priceA,'\n模型B預測房價區間:',preint_priceB,'\n模型C預測房價區間:',preint_priceC)
