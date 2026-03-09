data(package="POE5Rdata")

library(POE5Rdata)

data("star5_small", package="POE5Rdata")

head(star5_small)

names(star5_small)


# 建立子資料（去掉 aide）
star2 <- subset(star5_small, aide == 0)

head(star2)


# 2.22(a)
reg1 <- lm(totalscore ~ small, data = star2)
summary(reg1)
#2.22(b)
reg2 <- lm(readscore ~ small, data = star2)
summary(reg2)
#2.22(b)
reg3 <- lm(mathscore ~ small, data = star2)
summary(reg3)
#2.22(c) aide==1 regular==1
reg4 <- lm(totalscore ~ small + aide, data = star5_small)
summary(reg4)
reg5 <- lm(readscore ~ small + aide, data = star5_small)
summary(reg5)
reg6 <- lm(mathscore ~ small + aide, data = star5_small)
summary(reg6)
