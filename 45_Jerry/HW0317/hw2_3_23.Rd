# Problem a
library(POE5Rdata)
data("collegetown")
?collegetown

# PRICE  = alpha1+alpha2*SQFT^2 + e
mod1 <- lm(price~I(sqft^2),data=collegetown)
smod1 <- summary(mod1)
smod1
a1 <- coef(mod1)[1]
a2 <- coef(mod1)[2]
a2
sea1 <- coef(smod1)[1,2]
sea2 <- coef(smod1)[2,2]
sea2
tstar <- (a2-0.325)/sea2
tstar
alpha <- 0.05
df = df.residual(mod1)
tc <- qt(1-alpha,df)
tc
p <- 1-pt(tstar,df)
p

# Problem b
tstar <- (a2-0.1625)/sea2
tstar
alpha <- 0.05
df = df.residual(mod1)
tc <- qt(1-alpha,df)
tc
p <- 1-pt(tstar,df)
p

# problem c
x = 2000/100 # SQFT is in 100 square feet
a1
a2
vara1 <- vcov(mod1)[1,1]
vara2 <- vcov(mod1)[2,2]
cova1a2 <- vcov(mod1)[1,2]
L <- a1+a2*x^2
L
varL = vara1 + x^2*vara2+ 2*x*cova1a2
seL <- sqrt(varL)
alpha <- 0.05
tcr <- qt(1-alpha/2,df)
lowbL <- L - tcr*seL
upbL <- L + tcr*seL
lowbL
upbL

# Problem d
collegetown$price[which(collegetown$sqft==20)]
p20 <- mean(collegetown$price[which(collegetown$sqft==20)])
p20