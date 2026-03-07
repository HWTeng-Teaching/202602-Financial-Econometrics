#已將star5_small與cex5_small匯入
#第22題
#a
model_total<-lm(totalscore~small,data=star5_small)
summary(model_total)
#b readscore
model_read<-lm(readscore~small,data=star5_small)
summary(model_read)
#mathscore
model_math<-lm(mathscore~small,data=star5_small)
summary(model_math)
#c
model_total_aide<-lm(totalscore~aide,data=star5_small)
summary(model_total_aide)
#d readscore~aide
model_readscore_aide<-lm(readscore~aide,data=star5_small)
summary(model_readscore_aide)
#mathscore~aide
model_mathscore_aide<-lm(mathscore~aide,data=star5_small)
summary(model_mathscore_aide)

#第25題
#a
cex5<-cex5_small
hist(cex5$foodaway,main="Histogram of foodaway",xlab="foodaway")
summary(cex5$foodaway)
#b
foodaway_adv<-cex5$foodaway[cex5$advanced==1]
foodaway_col<-cex5$foodaway[cex5$college==1]
foodaway_nei<-cex5$foodaway[cex5$college==0 & cex5$advanced==0]
summary(foodaway_adv)
summary(foodaway_col)
summary(foodaway_nei)
#c
ln_foodaway<-log(cex5$foodaway)
hist(ln_foodaway,main="Histogram of ln(foodaway)",xlab="ln(foodaway)")
summary(ln_foodaway)
#d
cex5_sub<-cex5[cex5$foodaway > 0,]
model_ln<-lm(log(foodaway)~income,data=cex5_sub)
summary(model_ln)
#e
plot(cex5_sub$income,log(cex5_sub$foodaway),
     main="scatter plot with fitted line",xlab="income",ylab="ln(foodaway)",
     pch=19,col="gray")
abline(model_ln,col="red",lwd=2)
#f
res<-residuals(model_ln)
plot(cex5_sub$income,res,
     main="residuals vs. income",xlab="income",ylab="residuals",
     pch=19,col="gray")


