load("fultonfish.rdata")
library(AER)
#(a)
fishP.ols <- lm(lprice~mon+tue+wed+thu+stormy+mixed, data=fultonfish)
print(summary(fishP.ols))
cat("------------------------------------------------\n")
print(linearHypothesis(fishP.ols, c("stormy=0", "mixed=0")))
cat("------------------------------------------------\n")
b1 <- coef(fishP.ols)[[1]]
b2 <- coef(fishP.ols)[[2]]
b3 <- coef(fishP.ols)[[3]]
b4 <- coef(fishP.ols)[[4]]
b5 <- coef(fishP.ols)[[5]]
b6 <- coef(fishP.ols)[[6]]
b7 <- coef(fishP.ols)[[7]]

cat("Reduced form for ln Price:\n ln(Price)=",b1,"+ mon *",b2,"+ tue *",b3,"+ wed *",b4
    ,"+ thu *",b5,"+ stormy *",b6,"+ mixed *",b7)
#(b)(c)(d)
fish.iv <- ivreg(
  lquan ~ lprice + mon + tue + wed + thu |
    mon + tue + wed + thu + stormy + mixed,
  data = fultonfish
)
b12 <- coef(fish.iv)[[1]]
b22 <- coef(fish.iv)[[2]]
b32 <- coef(fish.iv)[[3]]
b42 <- coef(fish.iv)[[4]]
b52 <- coef(fish.iv)[[5]]
b62 <- coef(fish.iv)[[6]]
cat("\n")
cat("Demand eq (stormy, mixed are IVs):\n ln(Quantity)=",b12,"+  lprice*",b22,"+ mon *",b32,"+ tue *",b4
    ,"+ wed *",b52,"+ thu *",b62)
print(summary(fish.iv, diagnostics = TRUE))
