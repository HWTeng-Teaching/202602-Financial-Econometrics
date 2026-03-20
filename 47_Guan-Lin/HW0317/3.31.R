load("tuna.rdata")
#(a)
cat("(a)\n")
week <- matrix(1:52,nrow = 52)
cat("Summary Statistics of SAL1:\n")
print(summary(tuna$sal1))
cat("Standard deviation of SAL1:\n")
print(sd(tuna$sal1))
cat("Summary Statistics of APR1:\n")
print(summary(tuna$apr1))
cat("Standard deviation of APR1:\n")
print(sd(tuna$apr1))
par(mfrow=c(1,2))
plot(week, tuna$sal1,type="l",ylab="Sal 1",xlab="Week")
plot(week, tuna$apr1,type="l",ylab="Apr 1",xlab="Week")

#(b)
par(mfrow=c(1,1))
plot(tuna$apr1, tuna$sal1,ylab="Sal 1",xlab="Apr 1")

#(c)
cat("(c)\n")
price1 = 100 * tuna$apr1
mod1 <- lm(tuna$sal1 ~ price1)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat("SAL1 =",b1,"+",b2,"PRICE1 + e_i\n" )

N <- nrow(tuna)
alpha <- 0.05
df <- N-2
tcTwoTail <- qt(1-alpha/2, df)
seb2 <- sqrt(vcov(mod1)[2,2])
lower <- b2 - tcTwoTail * seb2
upper <- b2 + tcTwoTail * seb2
cat("Confidence Interval:[",lower,",",upper,"]\n")

#(d)
cat("(d)\n")
x0 = 70
ExpValue <- b1 + b2 * x0
cat("Expected Value:",ExpValue,"\n")
alphaD <- 0.1
tcTwoTail <- qt(1-alphaD/2, df)
sebExp <- sqrt(vcov(mod1)[1,1] + x0^2 * vcov(mod1)[2,2] + 2*x0 * vcov(mod1)[1,2])
lowerD <- ExpValue - tcTwoTail * sebExp
upperD <- ExpValue + tcTwoTail * sebExp
cat("Confidence Interval:[",lowerD,",",upperD,"]\n")

#(e)
cat("(e)\n")
PRICEMean <- 100 * mean(tuna$apr1)
SALMean <- mean(tuna$sal1)
alpha <- 0.05
tcTwoTail <- qt(1-alpha/2, df)
cat("Sample mean of price in cents:",PRICEMean,"\n")
cat("Sample mean of unit sales:",SALMean,"\n")
seEla <- PRICEMean / SALMean * seb2
lowerE <- PRICEMean / SALMean * b2 - seEla * tcTwoTail
upperE <- PRICEMean / SALMean * b2 + seEla * tcTwoTail
cat("Confidence Interval:[",lowerE,",",upperE,"]\n")

#(f)
cat("(f)\n")
alpha <- 0.1
nullEla <- -3 * SALMean / PRICEMean
t <- (b2-nullEla) / seb2
tc <- qt(1-alpha/2, df)
cat("Test Statistic:",t,"\nRejection Region:{t:|t| >",tc,"}\n")
p <- 2 * (1 - pt(abs(t), df))
cat("P-value: ",p,"\n")