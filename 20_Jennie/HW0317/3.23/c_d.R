load("D:/碩一下/計量經濟/作業/HW0317/23/collegetown.rdata")

#c
model=lm(price~I(sqft^2),data=collegetown)
summary(model)

newdata=data.frame(sqft=20)
predict(model,newdata,interval="confidence",level=0.95)

#d
subset_data=subset(collegetown,sqft==20)
mean_price=mean(subset_data$price)
mean_price
