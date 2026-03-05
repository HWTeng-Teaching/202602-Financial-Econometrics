#2.22
url<-"http://www.principlesofeconometrics.com/poe5/data/csv/star5_small.csv" 
star5_small<-read.csv(url)

#(a)
df_a<-subset(star5_small,aide==0)
model_a<-lm(totalscore~small,data=df_a)
summary(model_a)

#(b)
model_b_read<-lm(readscore~small,data=df_a)
model_b_math<-lm(mathscore~small,data=df_a)
summary(model_b_read)
summary(model_b_math)

#(c)
df_c<-subset(star5_small,small==0)
model_c<-lm(totalscore~aide,data=df_c)
summary(model_c)

#(d)
model_d_read<-lm(readscore~aide,data=df_c)
model_d_math<-lm(mathscore~aide,data=df_c)
summary(model_d_read)
summary(model_d_math)

#2.25
url<-"http://www.principlesofeconometrics.com/poe5/data/csv/cex5_small.csv" 
cex5_small<-read.csv(url)

#(a)
hist(cex5_small$foodaway,main='FOODAWAY', xlab='Expenditure($)')
summary(cex5_small$foodaway)
quantile(cex5_small$foodaway,probs=c(0.25,0.75))

#(b)
adv_group<-subset(cex5_small,advanced==1)
summary(adv_group$foodaway)

col_group<-subset(cex5_small,advanced==0&college==1)
summary(col_group$foodaway)

none_group<-subset(cex5_small,advanced==0&college==0)
summary(none_group$foodaway)

#(c)
cex5_small$ln_foodaway<-log(cex5_small$foodaway)
n_a <- nrow(cex5_small) 
cat("Observations in FOODAWAY (a):", n_a, "\n")
n_c <- sum(is.finite(cex5_small$ln_foodaway))
cat("Observations in ln(FOODAWAY) (c):", n_c, "\n")
hist(cex5_small$ln_foodaway)
summary(cex5_small$ln_foodaway)

#(d)
model_d<-lm(log(foodaway)~income,data=cex5_small,subset=(foodaway>0))
summary(model_d)

#(e)
df_valid<-subset(cex5_small,foodaway>0)
plot(df_valid$income, log(df_valid$foodaway),xlab="income",ylab='ln(foodaway)')
abline(model_d)

#(f)
res<-residuals(model_d)
plot(df_valid$income,res,xlab='income',ylab='residuals')
abline(h=0,lty=2)
