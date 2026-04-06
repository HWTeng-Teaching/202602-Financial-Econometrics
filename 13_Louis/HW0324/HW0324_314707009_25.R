rm(list=ls())

library(POE5Rdata)
library(tseries)

data("collegetown")

sqft_mean <- mean(collegetown$sqft)
price_mean <- mean(collegetown$price)

m1 <- lm(log(price)~sqft, data=collegetown)
m2 <- lm(log(price)~log(sqft), data=collegetown)
m3 <- lm(price~sqft, data=collegetown)

#(a)
summary(m1)
b2_m1 <- coef(m1)["sqft"]
slope_m1 <- b2_m1 * price_mean
elasticity_m1 <- b2_m1 * sqft_mean

cat(
  "(a)" ,"\n",
  "slope at the sample mean is : ",slope_m1, "\n",
  "elasticity at the sample mean is : ",elasticity_m1,"\n",
  sep=""
)

#(b)
summary(m2)
a2_m2 <- coef(m2)["log(sqft)"]
slope_m2 <- a2_m2 * (price_mean/sqft_mean)
elasticity_m2 <- a2_m2

cat(
  "(b)" ,"\n",
  "slope at the sample mean is : ",slope_m2, "\n",
  "elasticity at the sample mean is : ",elasticity_m2,"\n",
  sep=""
)

#(c)
cat(
  "(c)" ,"\n",
  "R-squared value from the linear model is : ",summary(m3)$r.squared, "\n",
  "Generalized R-squared from (b) is : ",cor(collegetown$price, exp(fitted(m2)))^2,"\n",
  "Generalized R-squared from (c)(the linear model) is : ",cor(collegetown$price, fitted(m3))^2,"\n",
  sep=""
)

#(d)
par(mfrow = c(1, 3))
hist(resid(m1), main = "Model (a) Residuals", xlab = "Residuals")
hist(resid(m2), main = "Model (b) Residuals", xlab = "Residuals")
hist(resid(m3), main = "Model (c) Residuals", xlab = "Residuals")

cat(
  "(d)" ,"\n",
  "the Jarque-Bera statistics for (a) is : ",jarque.bera.test(resid(m1))[[1]], "\n",
  "the Jarque-Bera statistics for (b) is : ",jarque.bera.test(resid(m2))[[1]],"\n",
  "the Jarque-Bera statistics for (c) is : ",jarque.bera.test(resid(m3))[[1]],"\n",
  sep=""
)

cat(
  "(d)" ,"\n",
  "p-value for (a) is : ",jarque.bera.test(resid(m1))[[3]], "\n",
  "p-value for (b) is : ",jarque.bera.test(resid(m2))[[3]],"\n",
  "p-value for (c) is : ",jarque.bera.test(resid(m3))[[3]],"\n",
  sep=""
)


#(e)
par(mfrow = c(1, 3))
plot(collegetown$sqft, resid(m1),
     main = "Model (a): Residuals vs SQFT",
     xlab = "SQFT", ylab = "Residuals")
abline(h = 0, lty = 2)
plot(collegetown$sqft, resid(m2),
     main = "Model (b): Residuals vs SQFT",
     xlab = "SQFT", ylab = "Residuals")
abline(h = 0, lty = 2)
plot(collegetown$sqft, resid(m3),
     main = "Model (c): Residuals vs SQFT",
     xlab = "SQFT", ylab = "Residuals")
abline(h = 0, lty = 2)


#(f)
house_2700 <- data.frame(sqft = 27) #房價單位是千美元，地坪以百為單位
cat(
  "(f)" ,"\n",
  "predict value for model (a) is : ",exp(predict(m1, house_2700))*1000, "\n",
  "predict value for model (b) is : ",exp(predict(m2, house_2700))*1000,"\n",
  "predict value for model (c) is : ",predict(m3, house_2700)*1000,"\n",
  sep=""
)

#(g)
pi_a = exp(predict(m1, newdata = house_2700, interval = "prediction", level = 0.95))
pi_b = exp(predict(m2, newdata = house_2700, interval = "prediction", level = 0.95))
pi_c = predict(m3, newdata = house_2700, interval = "prediction", level = 0.95)
cat(
  "(g): 95%Prediction Interval" ,"\n",
  "model (a) log-linear: ",pi_a[[2]],",",pi_a[[3]], "\n",
  "model (b) log-log: ",pi_b[[2]],",",pi_b[[3]],"\n",
  "model (c) linear : ",pi_c[[2]],",",pi_c[[3]],"\n",
  sep=""
)