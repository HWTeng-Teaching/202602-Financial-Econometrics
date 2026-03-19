# Problem a
library(POE5Rdata)
data("collegetown")
?collegetown

# PRICE  = beta1+betaa2*SQFT + e
mod1 <- lm(price~sqft,data=collegetown)
smod1 <- summary(mod1)
b1 <- coef(mod1)[1]
b2 <- coef(mod1)[2]
seb1 <- coef(smod1)[1,2]
seb2 <- coef(smod1)[2,2]

sqftmean <- mean(collegetown$sqft)
pricemean <- mean(collegetown$price)
elas <- b2*sqftmean/pricemean
elas
alpha <- 0.05
df = df.residual(mod1)
tc <- qt(1-alpha/2,df)
lowb2 <- b2 - tc  * seb2 
upb2 <- b2 + tc  * seb2 
lowelas <- lowb2*sqftmean/pricemean
lowelas
upelas <- upb2*sqftmean/pricemean
upelas

# Problem b
tstar <- (elas-1)/(seb2*sqftmean/pricemean)
tstar
alpha <- 0.01
tc <- qt(1-alpha/2,df)
tc
p <- 2 * (1-pt(tstar,df))
p

# Problem c
b2
seb2
tstar <- (b2-13)/seb2
tstar
alpha <- 0.05
tc <- qt(1-alpha,df)
tc
p <- 1-pt(tstar,df)
p

# problem d
x = 2000/100 # SQFT is in 100 square feet
b1
b2
varb1 <- vcov(mod1)[1,1]
varb2 <- vcov(mod1)[2,2]
covb1b2 <- vcov(mod1)[1,2]
L <- b1+b2*x
L
varL = varb1 + x^2*varb2+ 2*x*covb1b2
seL <- sqrt(varL)
alpha <- 0.05
tcr <- qt(1-alpha/2,df)
lowbL <- L - tcr*seL
upbL <- L + tcr*seL
lowbL
upbL

# Problem e
collegetown$price[which(collegetown$sqft==20)]
p20 <- mean(collegetown$price[which(collegetown$sqft==20)])
p20