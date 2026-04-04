load("collegetown.rdata")
library(tseries)
#(a)
cat("(a):\n")
mod1 <- lm(log(price)~sqft,data=collegetown)
beta1 <- coef(mod1)[[1]]
beta2 <- coef(mod1)[[2]]
cat("ln(Price) =",beta1,"+",beta2,"*SQFT + e\n")
SampleMean <- mean(collegetown$sqft)
cat("Slpoe:",exp(beta1+beta2*SampleMean)*beta2,"\n")
cat("Elasticity:",beta2*SampleMean,"\n")

#(b)
cat("(b):\n")
mod2 <- lm(log(price)~log(sqft),data=collegetown)
alpha1 <- coef(mod2)[[1]]
alpha2 <- coef(mod2)[[2]]
cat("ln(Price) =",alpha1,"+",alpha2,"*ln(SQFT) + e\n")
cat("Slpoe:",exp(alpha1)*alpha2*SampleMean^(alpha2-1),"\n")
cat("Elasticity:",alpha2,"\n")

#(c)
cat("(c):\n")
mod3 <- lm(price~sqft,data=collegetown)
delta1 <- coef(mod3)[[1]]
delta2 <- coef(mod3)[[2]]
RSquare1 <- cor(collegetown$sqft,collegetown$price)^2
cat("R^2 value from linear model:",RSquare1,"\n")
lnyhatb <- alpha1 + alpha2 * log(collegetown$sqft)
yhatb <- exp(lnyhatb)
GRSquareb <- cor(collegetown$price,yhatb)^2
cat("Generalized R^2 value from log-log model:",GRSquareb,"\n")
yhatc <- delta1 + delta2 * collegetown$sqft
GRSquarec <- cor(collegetown$price,yhatc)^2
cat("Generalized R^2 value from linear model:",GRSquarec,"\n")

#(d)
cat("(d):\n")
cat("log-linear model:\n")
hist(resid(mod1),
     main="Histogram of residual log-linear model",
     xlab="Residual",ylim=c(0,150))
print(jarque.bera.test(resid(mod1)))
cat("log-log model:\n")
hist(resid(mod2),
     main="Histogram of residual log-log model",
     xlab="Residual",ylim=c(0,150))
print(jarque.bera.test(resid(mod2)))
cat("linear model:\n")
hist(resid(mod3),
     main="Histogram of residual linear model",
     xlab="Residual",ylim=c(0,200))
print(jarque.bera.test(resid(mod3)))

#(e)
plot(collegetown$sqft,resid(mod1)
     ,xlab = "SQFT",ylab="Residual")
abline(h=0,lty=2)
plot(log(collegetown$sqft),resid(mod2)
     ,xlab = "ln(SQFT)",ylab="Residual")
abline(h=0,lty=2)
plot(collegetown$sqft,resid(mod3)
     ,xlab = "SQFT",ylab="Residual")
abline(h=0,lty=2)

#(f)
cat("(f):\n")
x <- 27
cat("Given sqft is 27 hundred square feet:\n")
y1 <- exp(beta1 + beta2 * x)
cat("Predicted value with log-linear model(in thousands):",y1,"\n")
y2 <- exp(alpha1 + alpha2 * log(x))
cat("Predicted value with log-log model(in thousands):",y2,"\n")
y3 <- delta1 + delta2 * x
cat("Predicted value with linear model(in thousands):",y3,"\n")
#(g)
cat("(g):\n")
alpha <- 0.05
N <- NROW(collegetown$sqft)
df <- N-2
tc <- qt(1-alpha/2,df)
xbar <- mean(collegetown$sqft)

#log-linear
cat("Log-linear model:\n")
sighatSqu1 <- summary(mod1)$sigma^2
varf1 <- sighatSqu1 * (1+1/N) + (x-xbar)^2 * vcov(mod1)[2,2]
sef1 <- sqrt(varf1)
lower1 <- exp(log(y1) - tc * sef1)
upper1 <- exp(log(y1) + tc * sef1)
cat("Prediction interval: [",lower1,",",upper1,"]\n")

#log-log
cat("Log-log model:\n")
zMat = log(collegetown$sqft)
z = log(x) 
lnXbar <- mean(zMat)
sighatSqu2 <- summary(mod2)$sigma^2
varf2 <- sighatSqu2 * (1+1/N) + (z-lnXbar)^2* vcov(mod2)[2,2]
sef2 <- sqrt(varf2)
lower2 <- exp(log(y2) - tc * sef2)
upper2 <- exp(log(y2) + tc * sef2)
cat("Prediction interval: [",lower2,",",upper2,"]\n")

#linear
cat("Linear model:\n")
sighatSqu3 <- summary(mod3)$sigma^2
varf3 <- sighatSqu3 * (1+1/N) + (x-xbar)^2 * vcov(mod3)[2,2]
sef3 <- sqrt(varf3)
lower3 <- y3 - tc * sef3
upper3 <- y3 + tc * sef3
cat("Prediction interval: [",lower3,",",upper3,"]\n")