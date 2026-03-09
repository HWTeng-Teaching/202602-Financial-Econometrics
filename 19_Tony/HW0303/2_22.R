rm(list=ls())

###Problem 2.22

library(PoEdata)
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/star5_small.rdata"   
dest <- file.path(tempdir(), "star5_small.rdata")
download.file(url, dest, mode = "wb") 
load(dest)

##a.
data_a <- star5_small$regular == 1 | star5_small$small == 1 


df <- star5_small[data_a, ]
mod1 <- lm(totalscore ~ small, data = df)
summary(mod1)

#The average total score representing students in a "regular class"
intercept <- coef(mod1)[1]

#The average difference in total score between "small class" and "regular class".
#If beta2 is positive and significant, it indicates that students in smaller classes perform better.
slope     <- coef(mod1)[2]

#Graph
plot(df$small, df$totalscore, 
     xlab="small", 
     ylab="totalscore",
     main = "Observations and fitted line",
     pch = 16, col = 'blue',
     type = "p")
abline(intercept, slope ,col = 'red')

cat(
  "b1 is: ", intercept,",when small=0, E[Totalscore|small=0]=b1", "\n",
  "b2 is: ", slope,",when small increase 1,totalscore increase b2", "\n",
  "Since b2>0, small class has higher totalscore, but the points in the two groups are widely dispersed.", "\n",
  "thus this may not Statistically significant", "\n",
  sep = ""
)    

##b.
#read
mod2 <- lm(readscore ~ small, data = df)
summary(mod2)
intercept_2 <- coef(mod2)[1]
slope_2     <- coef(mod2)[2]

#math
mod3 <- lm(mathscore ~ small, data = df)
summary(mod3)
intercept_3 <- coef(mod3)[1]
slope_3     <- coef(mod3)[2]

#Graph
par(mfrow = c(1, 2))
plot(df$small, df$readscore, 
     xlab="small", 
     ylab="readscore",
     main = "Readscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_2,slope_2 ,col = 'red')

plot(df$small, df$mathscore, 
     xlab="small", 
     ylab="mathscore",
     main = "Mathscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_3,slope_3 ,col = 'red')

par(mfrow = c(1, 1))

cat(
  "b1_read is: ", intercept_2,". b1_math is: ", intercept_3, "\n",
  "b2_read is: ", slope_2,". b2_math is: ", slope_3, "\n",
  "Readscore is more improved than Mathscore when class is small", "\n",
  "But it may not Statistically significant", "\n",
  sep = ""
)    


##c.
idx2 <- star5_small$regular == 1 | star5_small$aide == 1

df2 <- star5_small[idx2, ]
mod4 <- lm(totalscore ~ aide, data = df2)
summary(mod4)
intercept_4 <- coef(mod4)[1]
slope_4     <- coef(mod4)[2]

#Graph
plot(df2$aide, df2$totalscore, 
     xlab="aide", 
     ylab="totalscore",
     main = "Observations and fitted line",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_4,slope_4 ,col = 'red')

cat(
  "b1 is: ", intercept_4,", when aide=0, E[Totalscore|aide=0]=b1", "\n",
  "b2 is: ", slope_4,", when aide increase 1, totalscore increase b2", "\n",
  "Since b2>0, regular class with aide has higher totalscore, but the points in the two groups are widely dispersed.", "\n",
  "Thus this may not Statistically significant", "\n",
  sep = ""
)    

##d.
mod5 <- lm(readscore ~ aide, data = df2)
intercept_5 <- coef(mod5)[1]
slope_5     <- coef(mod5)[2]

mod6 <- lm(mathscore ~ aide, data = df2)
intercept_6 <- coef(mod6)[1]
slope_6     <- coef(mod6)[2]

###########################
par(mfrow = c(1, 2))
plot(df2$aide, df2$readscore, 
     xlab="aide", 
     ylab="readscore",
     main = "Readscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_5,slope_5 ,col = 'red')

plot(df2$aide, df2$mathscore, 
     xlab="aide", 
     ylab="mathscore",
     main = "Mathscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_6,slope_6 ,col = 'red')

par(mfrow = c(1, 1))

cat(
  "b1_read is: ", intercept_5,". b1_math is:", intercept_6, "\n",
  "b2_read is: ", slope_5,". b2_math is:", slope_6, "\n",
  "Readscore is more improved than mathscore when class has aide", "\n",
  "But it may not Statistically significant", "\n",
  sep = ""
)    