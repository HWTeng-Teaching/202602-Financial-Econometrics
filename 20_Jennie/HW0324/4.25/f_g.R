load("D:/碩一下/計量經濟/作業/HW0324/4.25/collegetown.rdata")

collegetown$ln_price=log(collegetown$price)
collegetown$ln_sqft=log(collegetown$sqft)
model_linear=lm(price~sqft,data=collegetown)
model_loglinear=lm(log(price)~sqft,data=collegetown)
model_loglog=lm(ln_price~ln_sqft,data=collegetown)

#f
predict_linear=coef(model_linear)[1]+coef(model_linear)[2]*27
predict_linear

predict_loglinear=exp(coef(model_loglinear)[1]+coef(model_loglinear)[2]*27)
predict_loglinear

predict_loglog=exp(coef(model_loglog)[1]+coef(model_loglog)[2]*log(27))
predict_loglog

#g
newdata=data.frame(sqft=27)
predict(model_linear, newdata, interval="prediction", level=0.95) 

newdata=data.frame(sqft = 27)
pred_loglinear=predict(model_loglinear, newdata, interval="prediction", level=0.95)
exp(pred_loglinear) 

newdata=data.frame(ln_sqft = log(27))
pred_loglog= predict(model_loglog, newdata, interval="prediction", level=0.95)
exp(pred_loglog) 
