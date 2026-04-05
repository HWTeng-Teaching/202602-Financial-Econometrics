rm(list=ls())
library(POE5Rdata)

#install.packages("tseries")
library(tseries)

#problem4.25
mod1 = lm(log(price) ~ sqft, data = collegetown)
mod2 = lm(log(price) ~ log(sqft), data = collegetown)
mod3 = lm(price ~ sqft, data = collegetown)

#(a)
summary(mod1)
# slope and elasticity at sample mean
slope_a = coef(summary(mod1))[[2]] * mean(collegetown$price)
elas_a = coef(summary(mod1))[[2]] * mean(collegetown$sqft)
cat(
  "(a)" ,"\n",
  "slope at the sample mean is : ",slope_a, "\n",
  "elasticity at the sample mean is : ",elas_a,"\n",
  sep=""
)

#(b)
summary(mod2)
# slope and elasticity at sample mean
slope_a = coef(summary(mod2))[[2]] * (mean(collegetown$price) / mean(collegetown$sqft))
elas_a = coef(summary(mod2))[[2]] 
cat(
  "(b)" ,"\n",
  "slope at the sample mean is : ",slope_a, "\n",
  "elasticity at the sample mean is : ",elas_a,"\n",
  sep=""
)

#(c)
cat(
  "(c)" ,"\n",
  "R-squared value from the linear model is : ",summary(mod3)$r.squared, "\n",
  "Generalized R-squared from (b) is : ",cor(collegetown$price, exp(fitted(mod2)))^2,"\n",
  "Generalized R-squared from (c)(the linear model) is : ",cor(collegetown$price, fitted(mod3))^2,"\n",
  sep=""
)

#(d)
par(mfrow = c(1, 3))
hist(resid(mod1), main = "Model (a) Residuals", xlab = "Residuals")
hist(resid(mod2), main = "Model (b) Residuals", xlab = "Residuals")
hist(resid(mod3), main = "Model (c) Residuals", xlab = "Residuals")

cat(
  "(d)" ,"\n",
  "the Jarque-Bera statistics for (a) is : ",jarque.bera.test(resid(mod1))[[1]], "\n",
  "the Jarque-Bera statistics for (b) is : ",jarque.bera.test(resid(mod2))[[1]],"\n",
  "the Jarque-Bera statistics for (c) is : ",jarque.bera.test(resid(mod3))[[1]],"\n",
  sep=""
)

cat(
  "(d)" ,"\n",
  "p-value for (a) is : ",jarque.bera.test(resid(mod1))[[3]], "\n",
  "p-value for (b) is : ",jarque.bera.test(resid(mod2))[[3]],"\n",
  "p-value for (c) is : ",jarque.bera.test(resid(mod3))[[3]],"\n",
  sep=""
)

#(e)
par(mfrow = c(1, 3))
plot(collegetown$sqft, resid(mod1),
     main = "Model (a): Residuals vs SQFT",
     xlab = "SQFT", ylab = "Residuals")
abline(h = 0, lty = 2)
plot(collegetown$sqft, resid(mod2),
     main = "Model (b): Residuals vs SQFT",
     xlab = "SQFT", ylab = "Residuals")
abline(h = 0, lty = 2)
plot(collegetown$sqft, resid(mod3),
     main = "Model (c): Residuals vs SQFT",
     xlab = "SQFT", ylab = "Residuals")
abline(h = 0, lty = 2)

#(f)
newhouse <- data.frame(sqft = 27) #單位是千美元
cat(
  "(f)" ,"\n",
  "predict value for model (a) is : ",exp(predict(mod1, newhouse))*1000, "\n",
  "predict value for model (b) is : ",exp(predict(mod2, newhouse))*1000,"\n",
  "predict value for model (c) is : ",predict(mod3, newhouse)*1000,"\n",
  sep=""
)

#(g)
pi_a = exp(predict(mod1, newdata = newhouse, interval = "prediction", level = 0.95))
pi_b = exp(predict(mod2, newdata = newhouse, interval = "prediction", level = 0.95))
pi_c = predict(mod3, newdata = newhouse, interval = "prediction", level = 0.95)
cat(
  "(f): 95%PI" ,"\n",
  "model (a) log-linear: ",pi_a[[2]],",",pi_a[[3]], "\n",
  "model (b) log-log: ",pi_b[[2]],",",pi_b[[3]],"\n",
  "model (c) linear : ",pi_c[[2]],",",pi_c[[3]],"\n",
  sep=""
)
