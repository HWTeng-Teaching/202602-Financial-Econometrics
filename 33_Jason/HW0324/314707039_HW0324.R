collegetown <- read.csv("http://www.principlesofeconometrics.com/poe5/data/csv/collegetown.csv")
#a小題
avg_PRICE<-mean(collegetown$price)
avg_SQFT<-mean(collegetown$sqft)
log_linear<-lm(log(price)~sqft,data=collegetown)
summary(log_linear)
slope_a<-coef(log_linear)[[2]]*avg_PRICE
elas_a<-coef(log_linear)[[2]]*avg_SQFT
cat("a小題斜率為:",slope_a,"\n")
cat("a小題彈性為:",elas_a,"\n")

#b小題
log_log<-lm(log(price)~log(sqft),data=collegetown)
summary(log_log)
slope_b<-coef(log_log)[[2]]*avg_PRICE/avg_SQFT
elas_b<-coef(log_log)[[2]]
cat("b小題斜率為:",slope_b,"\n")
cat("b小題彈性為:",elas_b,"\n")

#c小題
linear_linear<-lm(price~sqft,data=collegetown)
summary(linear_linear)
r2_linear <- summary(linear_linear)$r.squared
#為了將應變數變為一致，先得到ln(price)的預測值，再取exp()轉回原始價格單位
pred_price_loglog<-exp(predict(log_log))
# 計算實際價格與預測價格相關係數的平方
r2_gen_loglog <- cor(collegetown$price, pred_price_loglog)^2
cat("線性模型之R平方為:",r2_linear,"\n")
cat("對數-對數之廣義R平方為:",r2_gen_loglog,"\n")

#d小題
#要安裝並載入tseries套件，才能使用jarque.bera.test函數。
library(tseries)
res_a<-resid(log_linear)
res_b<-resid(log_log)
res_c<-resid(linear_linear)
#繪製直方圖觀察分佈
hist(res_a, main="Histogram of Residuals (Log-Linear)", col="lightblue", breaks=30)
hist(res_b, main="Histogram of Residuals (Log-Log)", col="lightgreen", breaks=30)
hist(res_c, main="Histogram of Residuals (Linear)", col="salmon", breaks=30)
#執行 Jarque-Bera 檢定
jb_a <-jarque.bera.test(res_a)
jb_b <-jarque.bera.test(res_b)
jb_c <-jarque.bera.test(res_c)

print(jb_a)
print(jb_b)
print(jb_c)

#e小題
#(a)Log-Linear模型的殘差圖
plot(collegetown$sqft,resid(log_linear),
     main="Residuals vs SQFT (Log-Linear)", 
     xlab="SQFT", ylab="Residuals", pch=20, col="blue")
abline(h=0, col="red", lwd=2) # 加入 y=0 基準線)
#(b)Log-Log模型的殘差圖
plot(log(collegetown$sqft),resid(log_log),
     main="Residuals vs log(SQFT) (Log-Log)", 
     xlab="log(SQFT)", ylab="Residuals", pch=20, col="darkgreen")
abline(h=0, col="red", lwd=2)
#(c)Linear模型的殘差圖
plot(collegetown$sqft, resid(linear_linear), 
     main="Residuals vs SQFT (Linear)", 
     xlab="SQFT", ylab="Residuals", pch=20, col="darkorange")
abline(h=0, col="red", lwd=2)

#f小題
new_house<-data.frame(sqft = 27)
pred_log_a<-predict(log_linear,newdata=new_house)
#取指數轉回原始房價
price_a<-exp(pred_log_a)

pred_log_b<-predict(log_log, newdata=new_house)
price_b<-exp(pred_log_b)

pred_c<-predict(linear_linear,newdata=new_house)
price_c<-pred_c
cat("Log-Linear 預測房價:", price_a, "\n")
cat("Log-Log    預測房價:", price_b, "\n")
cat("Linear     預測房價:", price_c, "\n")

#g小題
PI_a<-predict(log_linear, new_house, interval = "prediction", level = 0.95)
target_PI_a<-exp(PI_a)

PI_b<-predict(log_log, new_house, interval = "prediction", level = 0.95)
target_PI_b<-exp(PI_b)

target_PI_c<-predict(linear_linear, new_house, interval = "prediction", level=0.95)

cat("model a之預測區間下界為:",target_PI_a[2],"上界為:",target_PI_a[3],"\n")
cat("model b之預測區間下界為:",target_PI_b[2],"上界為:",target_PI_b[3],"\n")
cat("model c之預測區間下界為:",target_PI_c[2],"上界為:",target_PI_c[3],"\n")