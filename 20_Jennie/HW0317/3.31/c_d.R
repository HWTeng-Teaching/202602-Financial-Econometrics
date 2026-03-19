load("D:/碩一下/計量經濟/作業/HW0317/31/tuna.rdata")

#c
tuna$PRICE1=100*tuna$apr1
model=lm(sal1~PRICE1,data=tuna)
summary(model)

beta2=coef(model)["PRICE1"]
beta2

confint(model,"PRICE1",level=0.95)

#d
newdata=data.frame(PRICE1=70)
predict(model,newdata,interval="confidence",level=0.90)
