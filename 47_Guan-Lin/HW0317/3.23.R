load("collegetown.rdata")

#(a)
mod1 <- lm(price ~ I(sqft^2), data=collegetown)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat("Price =",b1,"+",b2,"sqft^2 + e_i\n" )

alpha <- 0.05
N <- nrow(collegetown)
#(a)
nullBeta2 <- 13/40 # $13000/40
df <- N-2
seb2 <- sqrt(vcov(mod1)[2,2])
tc <- qt(1-alpha, df) #critical region
t <- (b2 - nullBeta2)/ seb2 #test statistic
p <- 1 - pt(t,df)

cat("Degree of freedom:",df,"\n")
cat("(a): \n")
cat("P-value: ",p,"\n")
cat("Test Statistic:",t,"\nRejection Region:{t:t >",tc,"}\n")

#(b)
nullBeta2b <- 13/80
tb <- (b2 - nullBeta2b)/ seb2 #test statistic
pb <- 1 - pt(tb,df)
cat("(b): \n")
cat("P-value: ",pb,"\n")
cat("Test Statistic:",tb,"\nRejection Region:{t:t >",tc,"}\n")

#(c)
cat("(c): \n")
ExpVal <- b1 + b2 * 20^2
cat("Expected value: ",ExpVal,"\n")
seExp <- sqrt(vcov(mod1)[1,1] + 400^2 * vcov(mod1)[2,2] + 2*400 * vcov(mod1)[1,2])
tcTwoTail <- qt(1-alpha/2, df)
leftCI <-  ExpVal - tcTwoTail * seExp
rightCI <-  ExpVal + tcTwoTail * seExp
cat("Confidence Interval: [",leftCI,",",rightCI,"]\n")

#(d)
cat("(d): \n")
house20 <- subset(collegetown, sqft == 20)
theMean <- mean(house20$price)
cat("Sample mean: ",theMean)