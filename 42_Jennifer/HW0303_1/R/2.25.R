library(POE5Rdata)
data(cex5_small)
?cex5_small
#a
#直方圖
hist(cex5_small$foodaway,col = "grey")
#敘述統計
summary(cex5_small$foodaway)
#b
#條件篩選
#有大學教育
summary(cex5_small$foodaway[cex5_small$advanced==1])
#有進階教育
summary(cex5_small$foodaway[cex5_small$college==1])
#沒大學 + 沒進階教育
summary(cex5_small$foodaway[cex5_small$advanced==0&cex5_small$college==0])

#c
hist(log(cex5_small$foodaway),col = 'grey')
summary(log(cex5_small$foodaway))

#d
#log(0) 不存在
mod1<-lm(log(foodaway)~income,data=cex5_small[cex5_small$foodaway>0,])
smod1<-summary(mod1)
smod1
#e
plot(cex5_small$income[cex5_small$foodaway>0],log(cex5_small$foodaway[cex5_small$foodaway>0]),col="grey")

abline(mod1,col="blue",lwd=2)

#f
resids<-residuals(mod1)
plot(cex5_small$income[cex5_small$foodaway>0],resids,col="grey")
