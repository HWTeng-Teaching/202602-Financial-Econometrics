library(xtable)
library(knitr)

#23
#a
mod1<- lm(price ~ I(sqft^2), data = collegetown)
smod1<- summary(mod1)
smod1
alpha<- 0.05
table<- data.frame(xtable(mod1))
kable(table, caption = "Regression output showing the coefficients")

b2<- coef(mod1)[[2]]
seb2<- sqrt(vcov(mod1)[2,2]) 
df<- df.residual(mod1)
t<- (b2-0.325)/seb2
tcr<- qt(1-alpha, df)
table <- data.frame(t, tcr)
kable(table)

#b
mod1<- lm(price ~ I(sqft^2), data = collegetown)
smod1<- summary(mod1)
smod1
alpha<- 0.05
table<- data.frame(xtable(mod1))
kable(table, caption = "Regression output showing the coefficients")

b2<- coef(mod1)[[2]]
seb2<- sqrt(vcov(mod1)[2,2]) 
df<- df.residual(mod1)
t<- (b2-0.1625)/seb2
tcr<- qt(1-alpha, df)
p <- 1-pt(t, df)
table <- data.frame(t, tcr, p)
kable(table)

#c
alpha<- 0.05
x<- 20
mod1<- lm(price ~ I(sqft^2), data = collegetown)
tcr<- qt(1-alpha/2, df) # rejection region right of tcr.
df <- df.residual(mod1)
b1 <- mod1$coef[1]
b2 <- mod1$coef[2]
L<- b1+b2*(x^2)  # estimated L
varb1 <- vcov(mod1)[1, 1]
varb2 <- vcov(mod1)[2, 2]
covb1b2 <- vcov(mod1)[1, 2]
varL = varb1 + (x^4) * varb2 + 2*(x^2)*covb1b2 # var(L)
seL <- sqrt(varL) # standard error of L
lowbL <- L-tcr*seL
upbL <- L+tcr*seL
table <- data.frame(lowbL, upbL)
kable(table)

#d
houses_20 <- collegetown[collegetown$sqft == 20, ]
sample_mean_20 <- mean(houses_20$price)
