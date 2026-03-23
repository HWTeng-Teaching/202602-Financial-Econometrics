library(dplyr)

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

price<- collegetown$price
str(subset(collegetown,sqft == 20)$price)
summary(subset(collegetown,sqft == 20)$price)