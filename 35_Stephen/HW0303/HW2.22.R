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
