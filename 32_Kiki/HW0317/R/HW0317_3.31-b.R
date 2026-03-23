library(dplyr)

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))

sal1=tuna$sal1
apr1=tuna$apr1
plot(x=apr1,y=sal1,main="Price to sales",xlab="price",ylab="sales")