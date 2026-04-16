
# 2.22 -----------------
# install_github("ccolonescu/PoEdata")
rm(list=ls()) # Caution: this clears the Environment

# library(remotes)
install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
library(stargazer)

data('collegetown')

# a

mod1 <- lm(log(price) ~ sqft, data = collegetown)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
smod1 <- summary(mod1)
smod1
cat(b1,b2)

# b
mod2 <- lm(log(price) ~ log(sqft), data = collegetown)
b1 <- coef(mod2)[[1]]
b2 <- coef(mod2)[[2]]
cat(b1,b2)

# c 
mod3 <- lm(price ~ sqft, data = collegetown)
sm3 <- summary(mod3)
R2 <- sm3$r.squared

yhat <- predict(mod3)
cor(yhat,collegetown$price)^2

yhatb <- predict(mod2)
cor(exp(yhatb),collegetown$price)^2


# d
library(tseries)
e1 = mod1$residuals
e2 = mod2$residuals
e3 = mod3$residuals
jb1 = jarque.bera.test(e1)
jb2 = jarque.bera.test(e2)
jb3 = jarque.bera.test(e3)

hist(e1, col="grey", freq=F, main=c('JB=',jb1$statistic),
     ylab="density", xlab="ehat1")

hist(e2, col="grey", freq=F, main=c('JB=',jb2$statistic),
     ylab="density", xlab="ehat2")

hist(e3, col="grey", freq=F, main=c('JB=',jb3$statistic),
     ylab="density", xlab="ehat3")

# e.
par(mfrow=c(1,3))
plot(collegetown$sqft, e1)
plot(collegetown$sqft, e2)
plot(collegetown$sqft, e3)


#f.
predict(mod1)

#g.
pre_res1 = predict(mod1, newdata=data.frame(sqft=27), interval="prediction",level=0.95)
cat(exp(pre_res1[2]),exp(pre_res1[3]))

pre_res2 = predict(mod2, newdata=data.frame(sqft=27), interval="prediction",level=0.95)
cat(exp(pre_res2[2]),exp(pre_res2[3]))

pre_res3 = predict(mod3, newdata=data.frame(sqft=27), interval="prediction",level=0.95)
cat(pre_res3[2],pre_res3[3])


