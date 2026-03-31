#4.3
x=c(3,2,1,-1,0)
y=c(4,2,3,1,0)

#c
model=lm(y~x)

predict(model,newdata=data.frame(x=4),interval="prediction",level=0.95)

#d
predict(model,newdata=data.frame(x=4),interval="prediction",level=0.99)

#e
predict(model, newdata = data.frame(x = mean(x)), interval="prediction", level=0.95)
        