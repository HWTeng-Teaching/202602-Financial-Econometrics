#4.25
install.packages("xtable")
library(xtable)
install.packages("knitr")
library(knitr)
install.packages("tseries")
library(tseries)
#a
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))
mod1 <- lm(log(price)~sqft, data = collegetown) 
smod1<- summary(mod1)
tb1 <-data.frame(xtable(smod1))
kable (tb1, caption="Log-linear model for the priceprice equation")
 
#interpret
  #b1: expected ln(Price) when SQFT=0, not economically meaningfu;
  #b2=0.0360445: show the semi-elasticity
  #When SQFT increase 1 sqft, price increases 3.6%
Slope_a_mean <- coef(mod1)[2]*mean(collegetown$price)
#Slope_a_mean = b2*mean_y = 9.019661 
elasticity_a_mean <-coef(mod1)[2]*mean(collegetown$sqft)
#elasticity_a_mean = b2*mean_x = 0.9833701 

#b
mod2 <- lm(log(price)~log(sqft), data = collegetown) 
smod2<- summary(mod2)
tb2 <-data.frame(xtable(smod2))
kable (tb2, caption="Log-log model for the priceprice equation")
#interpret
#b1: expected ln(Price) when ln(SQFT)=0, not economically meaningfu;
#b2= 1.024828: show the elasticity
#A 1% increase in house size is estimated to increase house privce by 1.024%.

Slope_b_mean <- (coef(mod2)[2]*mean(collegetown$price))/mean(collegetown$sqft)
#Slope_b_mean = b2*y/x= 9.399923 
Elasticity_b <- coef(mod2)[2]
#Elasticity_b = b2 = 1.024828

#c
mod3 <- lm(price~sqft, data = collegetown) 
smod3<- summary(mod3)
rsq3 <-smod3$r.squared
#rsq3 = 0.6413167 linear
rsq1 <-smod1$r.squared
#rrsq1 = 0.5417259 log_linear
rsq2 <-smod2$r.squared
#rrsq2 = 0.4738445 log_log 
#Compare:
#Linear model (mod3) has highest R2
#However can not compare since dependent variables are different.

#d
ehat1 <- resid(mod1)
ehat2 <- resid(mod2)
ehat3 <- resid(mod3)
ebar1 <- mean(ehat1)
ebar2 <- mean(ehat2)
ebar3 <- mean(ehat3)
sde1 <-sd(ehat1)
sde2 <-sd(ehat2)
sde3 <-sd(ehat3)
qqnorm(ehat1)
qqnorm(ehat2)
qqnorm(ehat3)
hist(ehat1, col ="grey",freq=FALSE, main = "",
      ylab="density",xlab = "ehat1")
curve(dnorm(x,ebar1,sde1), col=2, add=TRUE,
      ylab="density", xlab = "ehat1")
hist(ehat2, col ="grey",freq=FALSE, main = "",
     ylab="density",xlab = "ehat2")
curve(dnorm(x,ebar2,sde2), col=2, add=TRUE,
      ylab="density", xlab = "ehat2")
hist(ehat3, col ="grey",freq=FALSE, main = "",
     ylab="density",xlab = "ehat3")
curve(dnorm(x,ebar3,sde3), col=2, add=TRUE,
      ylab="density", xlab = "ehat3")

#Jarque Bera Test
#H0: residuals are normally distributed
#H1: residuals are not normal

jarque.bera.test(ehat1)
#data:  ehat1
#X-squared = 26.679, df = 2, p-value = 1.61e-06

jarque.bera.test(ehat2)
#data:  ehat2
#X-squared = 14.279, df = 2, p-value = 0.0007933

jarque.bera.test(ehat3)
#data:  ehat3
#X-squared = 221.11, df = 2, p-value < 2.2e-16

#reject H0 in all 3 cases
#The residuals are not normally distributed in any of the models

#e  
plot(collegetown$sqft, ehat1, xlab="sqft", ylab = "residuals1")
plot(collegetown$sqft, ehat2, xlab="sqft", ylab = "residuals2")
plot(collegetown$sqft, ehat3, xlab="sqft", ylab = "residuals3")
#The residuals show some non-random patterns, especially mod3.
#Mod1 and mod2 display moderate dispersion.
#While mod3 show stronger pattern with larger spread. 

#f
str(collegetown)
sqftex = data.frame(sqft=27)
exp(predict(mod1, newdata=sqftex)) # 214.2336 
exp(predict(mod2, newdata=sqftex)) # 227.5386 
predict(mod3, newdata=sqftex) # 246.4557 

#g
exp(predict(mod1, newdata=sqftex, interval="prediction", level=0.95)) # [109.7685 ; 418.1167]
exp(predict(mod2, newdata=sqftex, interval="prediction", level=0.95)) # [111.1406 ; 465.8406]
predict(mod3, newdata=sqftex, interval="prediction", level=0.95) # [44.27727 ; 448.6341]

#h
#the linear model has highest R2, but its residual show stronger patterns and larger variance.
#the log_linear model provides a better fit in terms of residual behavior so it's preferred.