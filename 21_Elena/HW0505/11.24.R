setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("fultonfish.rdata")
library(AER)       
library(car) 

# (a)
reduced_price <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
summary(reduced_price)
linearHypothesis(reduced_price, c("stormy = 0", "mixed = 0"))


# (b)
demand_2sls <- ivreg(lquan ~ lprice + mon + tue + wed + thu |
                       mon + tue + wed + thu + stormy + mixed,
                     data = fultonfish)
summary(demand_2sls)


# (c)
summary(demand_2sls, diagnostics = TRUE)


# (d)
linearHypothesis(reduced_price, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
supply_2sls <- ivreg(lquan ~ lprice + stormy + mixed |
                       mon + tue + wed + thu + stormy + mixed,
                     data = fultonfish)
summary(supply_2sls)
