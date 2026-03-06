load("star5_small.rdata")
dataA = subset(star5_small,aide == 0)
#(a)
mod1 <- lm(totalscore ~ small, data=dataA)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat("TotalScore =",b1,"+",b2,"small + e_i\n" )

#(b)
mod2 <- lm(readscore ~ small, data=dataA)
b3 <- coef(mod2)[[1]]
b4 <- coef(mod2)[[2]]
cat("ReadScore =",b3,"+",b4,"small + e_i\n" )

mod3 <- lm(mathscore ~ small, data=dataA)
b5 <- coef(mod3)[[1]]
b6 <- coef(mod3)[[2]]
cat("MathScore =",b5,"+",b6,"small + e_i\n" )
dataC = subset(star5_small,small == 0)
#(c)
modc1 <- lm(totalscore ~ aide, data=dataC)
b1c <- coef(modc1)[[1]]
b2c <- coef(modc1)[[2]]
cat("TotalScore =",b1c,"+",b2c,"aide + e_i\n" )

#(d)
modc2 <- lm(readscore ~ aide, data=dataC)
b3c <- coef(modc2)[[1]]
b4c <- coef(modc2)[[2]]
cat("ReadScore =",b3c,"+",b4c,"aide + e_i\n" )

modc3 <- lm(mathscore ~ aide, data=dataC)
b5c <- coef(modc3)[[1]]
b6c <- coef(modc3)[[2]]
cat("MathScore =",b5c,"+",b6c,"aide + e_i\n" )