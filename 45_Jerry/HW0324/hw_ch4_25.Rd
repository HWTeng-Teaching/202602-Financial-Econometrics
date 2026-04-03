library(POE5Rdata)
data(collegetown)
?collegetown

# Problem a
# ln(price) = b1 + b2*SQFT + e
mod1 <- lm(log(price)~sqft,data=collegetown)
summary(mod1)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
x <- mean(collegetown$sqft) #sample mean
y <- exp(b1+b2*x)
DyDx <- b2*y  #slope at the sample mean
DyDx
PDyPDx <- b2*x #elasticity
PDyPDx

# Problem b
# ln(price) = b1 + b2*ln(SQFT) + e
mod2 <- lm(log(price)~log(sqft),data=collegetown)
summary(mod2)
a1 <- coef(mod2)[[1]]
a2 <- coef(mod2)[[2]]
x <- mean(collegetown$sqft) #sample mean
y <- exp(a1+a2*log(x))
DyDx <- a2*y/x  #slope at the sample mean
DyDx
PDyPDx <- a2 #elasticity
PDyPDx

# Problem c
smod1 <- summary(mod1)
smod2 <- summary(mod2)
sighat1 <- smod1$sigma^2
sighat2 <- smod2$sigma^2
yhat1 <- exp(b1+b2*collegetown$sqft+sighat1/2)
yhat2 <- exp(a1+a2*log(collegetown$sqft)+sighat2/2)
rg1 <- cor(collegetown$price,yhat1)^2
rg2 <- cor(collegetown$price,yhat2)^2
mod3 <- lm(price~sqft,data=collegetown)
yhat3 <- predict(mod3)
rg3 <- cor(collegetown$price,yhat3)^2
rg1
rg2
rg3

# Problem d
# install.packages("tseries")
library(tseries)
ehat1 <- resid(mod1)
hist(ehat1, col="grey", freq=FALSE, main="", ylab="density", xlab="ehat1")
jarque.bera.test(ehat1)

ehat2 <- resid(mod2)
hist(ehat2, col="grey", freq=FALSE, main="", ylab="density", xlab="ehat2")
jarque.bera.test(ehat2)

ehat3 <- resid(mod3)
hist(ehat3, col="grey", freq=FALSE, main="", ylab="density", xlab="ehat3")
jarque.bera.test(ehat3)

# problem e
plot(collegetown$sqft,ehat1)
plot(collegetown$sqft,ehat2)
plot(collegetown$sqft,ehat3)

# problem f
smod1 <- summary(mod1)
smod2 <- summary(mod2)
smod3 <- summary(mod3)
sighat21 <- smod1$sigma^2
sighat22 <- smod2$sigma^2
sighat23 <- smod3$sigma^2

d1 <- coef(mod3)[[1]]
d2 <- coef(mod3)[[2]]
pred1 <- exp(b1+b2*27+sighat21/2)
pred2 <- exp(a1+a2*log(27)+sighat22/2)
pred3 <- d1+d2*27
pred1
pred2
pred3

# problem g
alpha <- 0.05
df <- df.residual(mod1)
tcr <- qt(1-alpha/2,df)
tcr

N <- nobs(mod1) # number of observation, N
x <- 27
xbar <- mean(collegetown$sqft)

varb2 <- vcov(mod1)[2,2] #variance of b2
varf1 <- sighat21+sighat21/N+(x-xbar)^2*varb2
sef1 <- sqrt(varf1)
lb1 <- exp(b1+b2*x + sighat21/2 - tcr*sef1)
ub1 <- exp(b1+b2*x + sighat21/2 + tcr*sef1)
lb1
ub1

vara2 <- vcov(mod2)[2,2] #variance of a2
xbar_log <- mean(log(collegetown$sqft))
varf2 <- sighat22+sighat22/N+(log(x)-xbar_log)^2*vara2
sef2 <- sqrt(varf2)
lb2 <- exp(a1+a2*log(x) + sighat22/2 - tcr*sef2)
ub2 <- exp(a1+a2*log(x) + sighat22/2 + tcr*sef2)
lb2
ub2

vard2 <- vcov(mod3)[2,2] #variance of d2
varf3 <- sighat23+sighat23/N+(x-xbar)^2*vard2
sef3 <- sqrt(varf3)
lb3 <- d1+d2*x - tcr*sef3
ub3 <- d1+d2*x + tcr*sef3
lb3
ub3