load("D:/碩一下/計量經濟/作業/HW0324/4.25/collegetown.rdata")

#a
model=lm(log(price)~sqft,data=collegetown)
summary(model)

beta1=coef(model)[1]
beta2=coef(model)[2]
beta1
beta2
mean(collegetown$sqft)
mean(collegetown$price)

#b
collegetown$ln_price=log(collegetown$price)
collegetown$ln_sqft=log(collegetown$sqft)
model_loglog=lm(ln_price~ln_sqft,data=collegetown)

alpha1=coef(model_loglog)[1]
alpha2=coef(model_loglog)[2]
alpha1
alpha2

elasticity=alpha2
elasticity

#c
#R²
model_linear=lm(price~sqft,data=collegetown)
summary(model_linear)$r.squared

model_loglinear=lm(log(price)~sqft,data=collegetown)
summary(model_loglinear)$r.squared

model_loglog=lm(log(price)~log(sqft),data=collegetown)
summary(model_loglog)$r.squared
#generalized R²
y=collegetown$price

y_hat_linear=coef(model_linear)[1]+coef(model_linear)[2]*collegetown$sqft
R2_linear_gen=cor(y, y_hat_linear)^2
R2_linear_gen

y_hat_loglinear=exp(coef(model_loglinear)[1]+coef(model_loglinear)[2]*collegetown$sqft)
R2_loglinear_gen=cor(y, y_hat_loglinear)^2
R2_loglinear_gen


y_hat_loglog=exp(coef(model_loglog)[1]+coef(model_loglog)[2]*log(collegetown$sqft))
R2_loglog_gen=cor(y, y_hat_loglog)^2
R2_loglog_gen

#d
res_linear=resid(model_linear)
res_loglinear=resid(model_loglinear)
res_loglog=resid(model_loglog)

hist(res_linear, main="Linear Model Residuals", xlab="Residuals", col="lightblue", breaks=30)
hist(res_loglinear, main="Log-Linear Model Residuals", xlab="Residuals", col="lightblue", breaks=30)
hist(res_loglog, main="Log-Log Model Residuals", xlab="Residuals", col="lightblue", breaks=30)

jb_linear=jarque.bera.test(res_linear)
jb_loglinear=jarque.bera.test(res_loglinear)
jb_loglog=jarque.bera.test(res_loglog)

jb_linear
jb_loglinear
jb_loglog

#e
plot(collegetown$sqft, resid(model_linear),
     main="Residuals vs SQFT (Linear Model)",
     xlab="SQFT", ylab="Residuals")

plot(collegetown$sqft, resid(model_loglinear),
     main="Residuals vs SQFT (Log-Linear Model)",
     xlab="SQFT", ylab="Residuals")

plot(collegetown$sqft, resid(model_loglog),
     main="Residuals vs SQFT (Log-Log Model)",
     xlab="SQFT", ylab="Residuals")
