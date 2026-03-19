# Problem a
library(POE5Rdata)
data("tuna")
?tuna
summary(tuna$sal1)
sd(tuna$sal1)
summary(tuna$apr1)
sd(tuna$apr1)
plot(tuna$sal1,tuna$week,xlab = "week",ylab = "tuna$sal1")
plot(tuna$apr1,tuna$week,xlab = "week",ylab = "tuna$apr1")

# Problem b
plot(tuna$sal1,tuna$apr1)

# Problem c
price1 = 100*tuna$apr1
mod1 <- lm(tuna$sal1~price1)
smod1 <- summary(mod1)
smod1
b2 <- coef(mod1)[2]
b2
seb2 <- coef(smod1)[2,2]
alpha <- 0.05
df = df.residual(mod1)
tcr <- qt(1-alpha/2,df)
lowbb2 <- b2 - tcr*seb2
upbb2 <- b2 + tcr*seb2
lowbb2
upbb2

# problem d
x = 70
b1
b2
varb1 <- vcov(mod1)[1,1]
varb2 <- vcov(mod1)[2,2]
covb1b2 <- vcov(mod1)[1,2]
L <- b1+b2*x
L
varL = varb1 + x^2*varb2+ 2*x*covb1b2
seL <- sqrt(varL)
alpha <- 0.1
tcr <- qt(1-alpha/2,df)
lowbL <- L - tcr*seL
upbL <- L + tcr*seL
lowbL
upbL

# Problem e
pricemean <- mean(price1)
pricemean
salmean <- mean(tuna$sal1)
salmean
elas <- b2 * pricemean/salmean
elas
alpha <- 0.05
df = df.residual(mod1)
tc <- qt(1-alpha/2,df)
seelas = pricemean/salmean * seb2
lowelas <- elas - tc  * seelas
upelas <- elas + tc  * seelas
lowelas
upelas

# Problem f
alpha <- 0.1
tstar = (elas-(-3))/seelas
elas
seelas
tstar
tc <- qt(1-alpha/2,df)
tc
lowelas <- elas - tc  * seelas
upelas <- elas + tc  * seelas
lowelas
upelas
p <- 2*pt(tstar,df)
p