rm(list=ls())
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
install.packages("stargazer")
library(stargazer)
data('star5_small')
head(star5_small)

#2.22.a
table_22_a <- lm(totalscore ~ small, data = star5_small)
summary(table_22_a)

#2.22.b1
table_22_b1 <- lm(readscore ~ small, data = star5_small)
summary(table_22_b1)

#2.22.b2
table_22_b2 <- lm(mathscore ~ small, data = star5_small)
summary(table_22_b2)

#2.22.c
table_22_c <- lm(totalscore ~ aide, data = star5_small)
summary(table_22_c)

#2.22.d1
table_22_d1 <- lm(readscore ~ aide, data = star5_small)
summary(table_22_d1)

#2.22.d2
table_22_d2 <- lm(mathscore ~ aide, data = star5_small)
summary(table_22_d2)

data('cex5_small')
head(cex5_small)

#2.25.a
summary(cex5_small$foodaway)
hist(cex5_small$foodaway, main = "histogram of foodaway")

#2.25.b1
summary(cex5_small$foodaway[cex5_small$advanced == 1])

#2.25.b2
summary(cex5_small$foodaway[cex5_small$college == 1])

#2.25.b3
summary(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0])

#2.25.c
cex5_small$lnfoodaway = ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)
hist(cex5_small$lnfoodaway, main = "histogram of ln(foodaway)")
summary(cex5_small$lnfoodaway)

#2.25.d
table_25_d <- lm(lnfoodaway ~ income, data = cex5_small)
summary(table_25_d)

#2.25.e
plot(cex5_small$income, cex5_small$lnfoodaway, xlab = "income(in $100)", ylab = "ln(foodaway)", pch = 20)
abline(table_25_d, col = "red", lwd = 2)

#2.25.f
e_resid <- residuals(table_25_d)
plot(table_25_d$model$income, e_resid, xlab = "income(in $100)", ylab = "residuals", pch = 20, col = "blue")