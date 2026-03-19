b0=6.855
b1=3.880
#a
x=seq(0,100,1)
y=b0+b1*x

plot(x,y,
     type="l",
     xlab="INCOME (thousand $)",
     ylab="INSURANCE (thousand $)",
     main="Fitted Relationship",
     col="blue")
points(59.3,236.939,pch=19)

#b
n=20
df=n-2
seb0=7.383
seb1=0.112
t=qt(0.975,df)

lower=b1-t*seb1
upper= b1+t*seb1

lower
upper

#c
cov_b0b1=-0.746
xi=100000/1000
yi=b0+b1*xi
yi

var_yi=seb0^2+xi^2*seb1^2+2*xi*cov_b0b1
se_yi=sqrt(var_yi)
t99=qt(0.995,df)

lower99=yi-t99*se_yi
upper99= yi+t99*se_yi

lower99
upper99
