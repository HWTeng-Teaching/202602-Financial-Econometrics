rm(list=ls())

# 1. 
#(a)
library(tidyverse)
x = c(3, 2, 1, -1, 0)
y = c(4, 2, 3, 1, 0)
x_bar = mean(x)
y_bar = mean(y)
df=data.frame(x,y)
df = df  |> 
  mutate(
    x_minus_xbar = x - x_bar,
    x_minus_xbar_sq = (x - x_bar)^2,
    y_minus_ybar = y - y_bar,
    prod = x_minus_xbar * y_minus_ybar
  )
df1 = rbind(df, Total = colSums(df))
#(b)
b2 = sum(df$prod) / sum(df$x_minus_xbar_sq)
b1 = y_bar - b2 * x_bar
b1
b2
#(c)
N = nrow(df)
sum(x*x)
sum(x*y)
sum(df$x_minus_xbar_sq) 
(sum(x*x)-N*x_bar**2)
sum(df$prod)
(sum(x*y)-N*x_bar*y_bar)
#(d)
df2=data.frame(x,y)
df2 = df2  |> 
  mutate(
    y_hat = b1 + b2 * x,              
    e_hat = y - y_hat,                 
    e_hat_sq = e_hat^2,                
    x_e_hat = x * e_hat
  )
df2 = rbind(df2, Total = colSums(df2))

s2x=var(x)
s2y=var(y)
sxy=cov(x, y)
sx = sd(x)
sy = sd(y)
rxy=cor(x, y)
cvx=100 * (sx / x_bar)
median(x)

#(e)
plot(x, y)                 

abline(a = b1, b = b2, 
       col = "red",                                  
       lwd = 2)
#(f)
points(x_bar, y_bar, col = "green", pch = 19, cex = 2)

#22.
library (devtools)
install_git("https://github.com/ccolonescu/POE5Rdata")
library(POE5Rdata)
data(star5_small)
#(a)
star_filt = star5_small |> filter(small == 1 | regular == 1)
model1 = lm(totalscore ~ small, data = star_filt)
coef(model1)[[1]]
coef(model1)[[2]]
summary(model1)
#(b)
model2 = lm(readscore ~ small, data = star_filt)
coef(model2)[[1]]
coef(model2)[[2]]
summary(model2)

model3 = lm(mathscore ~ small, data = star_filt)
coef(model3)[[1]]
coef(model3)[[2]]
summary(model3)

#(c)
star_filt2 = star5_small |> filter(aide == 1 | regular == 1)
model21 = lm(totalscore ~ aide, data = star_filt2)
coef(model21)[[1]]
coef(model21)[[2]]
summary(model21)

#(d)
model22 = lm(readscore ~ aide, data = star_filt2)
coef(model22)[[1]]
coef(model22)[[2]]
summary(model22)

model23 = lm(mathscore ~ aide, data = star_filt2)
coef(model23)[[1]]
coef(model23)[[2]]
summary(model23)
#---------------------------------
#25.
data(cex5_small)
#(a)
hist(cex5_small$foodaway, main="Food Away")
summary(cex5_small$foodaway)

#(b)
cex5_small_adv = cex5_small |> filter(advanced==1)
cex5_small_coll = cex5_small |> filter(college==1)
cex5_small_Nac = cex5_small |> filter(advanced==0 & college==0)
summary(cex5_small_adv$foodaway)
summary(cex5_small_coll$foodaway)
summary(cex5_small_Nac$foodaway)

#(c)
cex5_small=cex5_small |> filter(foodaway>0)
cex5_small <- cex5_small  |> 
  mutate(ln_foodaway = log(foodaway))
hist(cex5_small$ln_foodaway, main="Food Away")
#(d)
model_log = lm(log(foodaway) ~ income, data = cex5_small)
summary(model_log)


#(e)
plot(log(foodaway)~income,data = cex5_small)
abline(model_log, col = "red", lwd = 2)

#(f)
r =  resid(model_log)

plot(cex5_small$income,r)

abline(h = 0, col = "red", lty = 2)
