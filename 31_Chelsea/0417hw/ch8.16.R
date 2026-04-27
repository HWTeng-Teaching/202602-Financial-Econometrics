data(package='POE5Rdata')
library(POE5Rdata)
data('vacation',package='POE5Rdata')
model<-lm(miles~income+age+kids,data=vacation)
summary(model)
#(a)
confint(model,'kids',level=0.95)
#(b)
res<-resid(model)
plot(vacation$income,res,xlab='income',ylab='residual',main='income v.s residuals')
abline(h=0)
plot(vacation$age,res,xlab='age',ylab='residual',main='age v.s residuals')
abline(h=0)
#(c)
vacation_sorted<-vacation[order(vacation$income),]
low<-vacation_sorted[1:90, ]
high<-vacation_sorted[(nrow(vacation_sorted)-89):nrow(vacation_sorted), ]
model_low<-lm(miles~income+age+kids,data=low)
SSE_low<-sum((resid(model_low))^2)
df_low<-df.residual(model_low)

model_high<-lm(miles~income+age+kids,data=high)
SSE_high<-sum((resid(model_high))^2)
df_high<-df.residual(model_high)

F_test<-(SSE_high/df_high)/(SSE_low/df_low)
pvalue<-1-pf(F_test,df_high,df_low)
cat("F-statistic =", F_test, "\np-value =", pvalue, "\n")

#(d)
install.packages('lmtest')
install.packages('sandwich')
library(lmtest)
library(sandwich)
#robust variance
robust_se <- vcovHC(model, type = "HC1")
coeftest(model, vcov=robust_se)
beta<-coef(model)['kids']
se<-sqrt(robust_se['kids','kids'])
lower<-beta-1.96*se
higher<-beta+1.96*se
cat('beta 95% CI',lower,higher)
#(e)
model_gls<-lm(miles~income+age+kids,data=vacation,weights=1/(income^2))
summary(model_gls)
confint(model_gls,'kids',level=0.95)
coeftest(model_gls,vcov=vcovHC(model_gls,type='HC1'))
