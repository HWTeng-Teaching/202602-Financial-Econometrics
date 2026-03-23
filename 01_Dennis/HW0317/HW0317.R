




rm(list=ls()) # Caution: this clears the Environment

# library(remotes)
# install_github("ccolonescu/POE5Rdata")


# 3.18-----------------
# install_github("ccolonescu/PoEdata")
x = 1:100
y = 6.855 + 3.880 * x

plot(x,y,type='l')
points(59.3,236.939,col='red',pch=19)




alpha = 0.01
df = 20-2
qt(1-alpha/2,df)
qt(1-alpha,df)


# 3.23----------------------
library(POE5Rdata)
library(stargazer)

data('collegetown')


mod = lm(price~I(sqft^2),data=collegetown)
summary(mod)
vcov(mod)

alpha = 0.05
df = 498
qt(1-0.05,df)
1-pt(-26.73,df)
1-pt(4.1893,df)

a1 = mod$coefficients[1]
a2 = mod$coefficients[2]
x = 20
L = a1 + a2*x^2
L
varb1 = vcov(mod)[1,1]
varb2 = vcov(mod)[2,2]
covb1b2 = vcov(mod)[1,2]
varL = varb1 + x^4*varb2 + 2*x^2*covb1b2
sqrt(varL)
alpha = 0.05
df = 500-2
qt(1-alpha/2,df)

167.3735-1.965*4.75


#d. CI in repeated sampling
ci_l = 158.0397
ci_h = 176.7073

d_q = collegetown[collegetown[,2]==20,]
mean(d_q$price)



# 3.31----------------------
library(POE5Rdata)
library(stargazer)
library(psych)
data('tuna')
tuna_mini = tuna[,c(1,2)]

describe(tuna_mini)


plot(tuna_mini$sal1,type='l',xlab = 'week',ylab='SAL1')
plot(tuna_mini$apr1,type='l',xlab = 'week',ylab='APR1')
plot(tuna_mini$apr1,tuna_mini$sal1,xlab = 'APR1',ylab='SAL1')

#c.
tuna_mini$price1 = tuna_mini$apr1*100
mod = lm(sal1~price1,data=tuna_mini)
summary(mod)
df = 52-2
alpha = 0.05
qt(1-alpha/2,df)

#d
vcov(mod)


alpha = 0.10
qt(1-alpha/2,df)

#e.
mean(tuna_mini$sal1)
mean(tuna_mini$price1)

alpha = 0.05
qt(1-alpha/2,df)


#f.
b2 = mod$coefficients[2]
pt(-2.25,df)



