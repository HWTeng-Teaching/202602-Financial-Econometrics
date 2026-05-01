load("D:/碩一下/計量經濟/作業/HW0428/11.24/fultonfish.rdata")
ls()
names(fultonfish)

#a
model=lm(lprice ~ mon + tue + wed + thu + stormy + mixed,
            data = fultonfish)
summary(model)

library(carData)
linearHypothesis(model, c("stormy = 0", "mixed = 0"))

#b
first=lm(lprice ~ mon + tue + wed + thu + stormy + mixed,
            data = fultonfish)
fultonfish$price_hat=fitted(first)

second=lm(lquan ~ price_hat + mon + tue + wed + thu,
             data = fultonfish)
summary(second)

#c
library(AER)

iv_model=ivreg(lquan ~ lprice + mon + tue + wed + thu |
                    mon + tue + wed + thu + stormy + mixed,
                  data = fultonfish)

summary(iv_model, diagnostics = TRUE)

#d
linearHypothesis(model,
                 c("mon = 0",
                   "tue = 0",
                   "wed = 0",
                   "thu = 0"))
