rm(list=ls())
# install.packages('tidyr')
library(POE5Rdata)
library(dplyr)
library(tidyr)
library(tibble)
######################
#problem3.31
#(a)
desc_table <- tuna %>%
  summarise(
    across(
      c(sal1, apr1),
      list(
        N = ~sum(!is.na(.)),
        mean = ~mean(., na.rm = TRUE),
        `Std. dev.` = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_sep = "_"
  )
print
plot(seq(1,52), tuna$sal1, 
     xlab="week", 
     ylab="number of cans of brand #1 sold during week",
     main = "sal1 vs week",
     col = 'blue',
     type = "l")
plot(seq(1,52), tuna$apr1, 
     xlab="week", 
     ylab="average price per can of brand #1 during week",
     main = "apr1 vs week",
     col = 'blue',
     type = "l")
cat(
  "3.31(a)", "\n",
  "weekly sales exhibit much greater variation than weekly price.", "\n",
  sep = ""
)  
#(b)
plot(tuna$apr1, tuna$sal1, 
     xlab="average price per can of brand #1 during week", 
     ylab="number of cans of brand #1 sold during week",
     main = "sal1 vs apr1",
     pch = 16,col = 'blue',
     type = "p")
cat(
  "3.31(b)", "\n",
  "There is an inverse relationship between the weekly sales and weekly price.", "\n",
  "it is As expected, the higher the price, the lower the sales volume.", "\n",
  sep = ""
)    
#(c)
price1 = 100 * tuna$apr1
mod1 = lm(tuna$sal1 ~ price1)
b1 = coef(mod1)[1]
b2 = coef(mod1)[2]  #加1美分銷量減少b2罐

alpha = 0.05
df = df.residual(mod1)              #degree of freedom
tc = qt(1-alpha/2, df)                #critical points
smod1 = summary(mod1)
se_b2 = coef(smod1)[2,2]           
lowb = b2 - tc*se_b2
upb  = b2 + tc*se_b2

cat(
  "3.31(c)", "\n",
  "a one-cent increase in the price of brand one will reduce the expected", "\n",
  "weekly sales of brand one tuna by ",-b2," cans", "\n",
  "and 95% CI estimate is [",lowb,",",upb,"]","\n",
  sep=""
)
#(d)
# price1 = 70
e_hat = b1 + b2 * 70

alpha_d = 0.1
tc_d = qt(1-alpha_d/2, df)                #critical points
se_l = sqrt(vcov(mod1)[1,1] + 4900*vcov(mod1)[2, 2] + 140*vcov(mod1)[1,2])

lowb_d = e_hat - tc_d*se_l
upb_d  = e_hat + tc_d*se_l

cat
#(e)
mod2 = lm(tuna$sal1 ~ tuna$apr1)
b2_e = coef(mod2)[2]
smod2 = summary(mod2)

alpha_e = 0.05
c = mean(tuna$apr1) / mean(tuna$sal1)
ela_hat = b2_e * c
se_ela = coef(smod2)[2,2] * abs(c)
tc_e = qt(1-alpha_e/2, df)         #自由度不變所以沒改

lowb_e = ela_hat - tc_e*se_ela
upb_e  = ela_hat + tc_e*se_ela

cat(
  "3.31(e)", "\n",
  "estimate of the elasticity is ",ela_hat, "\n",
  "The 95% CI estimate is [",lowb_e,",",upb_e,"]","\n",
  "since absolute value of elasticity is ",abs(ela_hat), "\n",
  "this is elastic and hence the result satisfied Law of Demand and Supply", "\n",
  sep=""
)

#(f)
# test statistic is (ela_hat + 3)/ (c *se(b2))
alpha_e = 0.1
t_test = (ela_hat + 3) / se_ela
tc_f = qt(1-alpha_e/2, df) 

# state the null and alternative hypotheses in terms of the model parameters
# 這邊會另外寫
# h0 = 3,h1 != 3

cat(
  "3.31(f)", "\n",
  "the rejection region is tc > ",tc_f," or tc < ",-tc_f, "\n",
  "and test t-value is ",t_test, "\n",
  "hence We reject the null hypothesis", "\n",
  sep=""
)
