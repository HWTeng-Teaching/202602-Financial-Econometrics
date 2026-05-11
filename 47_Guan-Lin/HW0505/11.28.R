load("truffles.rdata")
library(systemfit)
library(broom)
library(knitr)
#(b)
cat("(b): \n")
D <- p~q+ps+di
S <- p~q+pf
sys <- list(D,S)
instr <- ~ps+di+pf
truff.sys <- systemfit(sys, inst=instr, 
                       method="2SLS", data=truffles)
print(summary(truff.sys))
#(c)
cat("(c): \n")
Pbar <- mean(truffles$p)
Qbar <- mean(truffles$q)
ela <- Pbar /coef(truff.sys)[[2]] / Qbar 
cat("Elasticity for demand function:",ela,"\n")

#(d)
q_seq <- seq(0, 40, length.out=100)
Pd <- (coef(truff.sys)[[1]] +  coef(truff.sys)[[2]] * q_seq 
       + coef(truff.sys)[[3]] * 22 + coef(truff.sys)[[4]] * 3.5)
Ps <- coef(truff.sys)[[5]] +  coef(truff.sys)[[6]] * q_seq + coef(truff.sys)[[7]] * 23

plot(q_seq, Pd, type="l",lwd=2,xlab="Quantity", ylab="Price",main="Supply and Demand")
lines(q_seq, Ps, lwd=2, lty=2)
legend("topright",legend=c("Demand","Supply"),lty=c(1,2), lwd=2)

#(e)
cat("(e): \n")
c1 <- coef(truff.sys)[[1]] + coef(truff.sys)[[3]] * 22 + coef(truff.sys)[[4]] * 3.5
c2 <- coef(truff.sys)[[5]] + coef(truff.sys)[[7]] * 23
a2 <- coef(truff.sys)[[2]]
b2 <- coef(truff.sys)[[6]]
P <- (a2 * c2 - b2 * c1) / (a2 - b2)
Q <- (c2 - c1) / (a2 - b2)
cat("Equilibrium Quantity:", Q,"\nEquilibrium Price:",P,"\n")

Q.red <- lm(q~ps+di+pf, data=truffles)
pi1 <- coef(Q.red)
P.red <- lm(p~ps+di+pf, data=truffles)
pi2 <- coef(P.red)

ReducedQ <- pi1[[1]] + 22 * pi1[[2]] + 3.5 * pi1[[3]] + 23 * pi1[[4]]
ReducedP <- pi2[[1]] + 22 * pi2[[2]] + 3.5 * pi2[[3]] + 23 * pi2[[4]]
cat("Reduced-form Quantity:", ReducedQ,"\nReduced-form Price:",ReducedP,"\n")

#(f)
cat("(f): \n")
Demand <- lm(p~q+ps+di,data = truffles)
Supply <- lm(p~q+pf,data = truffles)
cat("Summary statistic for demand:\n")
print(summary(Demand))
cat("Summary statistic for supply:\n")
print(summary(Supply))
