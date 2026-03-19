load("D:/碩一下/計量經濟/作業/HW0317/31/tuna.rdata")
head(tuna)

#a
mean(tuna$sal1)
min(tuna$sal1)
max(tuna$sal1)
sd(tuna$sal1)

mean(tuna$apr1)
min(tuna$apr1)
max(tuna$apr1)
sd(tuna$apr1)

tuna$week=1:nrow(tuna)
plot(tuna$week,tuna$sal1,type = "l",col="blue",
     xlab="Week",ylab="Sales of Brand 1",main="Weekly Sales of Brand 1")
plot(tuna$week,tuna$apr1,type = "l",col="blue",
     xlab="Week",ylab="Price per Can ($)", main="Weekly Price of Brand 1")

#b
plot(tuna$apr1,tuna$sal1, 
     xlab="Price per Can ($)", 
     ylab="Weekly Sales of Brand 1",
     main="Weekly Sales vs Price of Brand 1",
     pch=16, col="blue")
