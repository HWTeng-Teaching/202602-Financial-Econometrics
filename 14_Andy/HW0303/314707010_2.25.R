rm(list=ls())

#install.packages("devtools")
#library(devtools)
#install_github("ccolonescu/PoEdata")
#library(PoEdata)
######################

#problem 2.25
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"   
dest <- file.path(tempdir(), "cex5_small.rdata")

download.file(url, dest, mode = "wb") 
load(dest)

#(a)
cat(
  "Mean of FOODWAY is:", mean(cex5_small$foodaway), "\n",
  "Median of FOODWAY is:", median(cex5_small$foodaway), "\n",
  "25th percentiles of FOODWAY is:", quantile(cex5_small$foodaway, 0.25), "\n",
  "75th percentiles of FOODWAY is:", quantile(cex5_small$foodaway, 0.75), "\n",
  sep = " "
)    
## Mean of FOODWAY is: 49.27085 
##  Median of FOODWAY is: 32.555 
##  25th percentiles of FOODWAY is: 12.04 
##  75th percentiles of FOODWAY is: 67.5025
hist(cex5_small$foodaway,xlab = 'foodaway from home expenditure per month per person past quarte', ylab = 'frequency', main = 'Histogram of FOODAWAY',
     xlim = c(0,1000), breaks= 20)
#資料集：cex5_small
#變數：foodaway
#xlim -> x軸的範圍(設定從0到1000)
#breaks -> 直方圖設定成幾個區間

#(b)
#cat -> 把文字與計算結果連接在一起並直接輸出到console
cat(
  "advanced = 1:", "\n",
  "N:", sum(cex5_small$advanced), "\n", #sum() 代表資料中=1的個數(因為advanced degree為dummy variable)
  "Mean:", mean(cex5_small$foodaway[cex5_small$advanced == 1]), "\n", #只取advanced = 1的人並計算foodaway的平均值
  "Median", median(cex5_small$foodaway[cex5_small$advanced == 1]), "\n",
  "college = 1:", "\n",
  "N:", sum(cex5_small$college), "\n",
  "Mean:", mean(cex5_small$foodaway[cex5_small$college == 1]), "\n",
  "Median:", median(cex5_small$foodaway[cex5_small$college == 1]), "\n",
  "None:", "\n",
  "N:", sum(cex5_small$advanced == 0 & cex5_small$college == 0), "\n",  
  "Mean:", mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0]), "\n",
  "Median:", median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0]), "\n",
  sep = "" #代表字串之間不要自動加空格。如果沒有設定，R 會自動加入空格
)        
## advanced = 1:
## N:257
## Meam:73.15494
## Median48.15
## college = 1:
## N:369
## Mean:48.59718
## Median:36.11
## None:
## N:574
## Mean:39.01017
## Median:26.02

#(c)
hist(log(cex5_small$foodaway),prob = TRUE,xlab = 'foodaway', ylab = 'Percent', main = 'Histogram of FOODAWAY',
     xlim = c(-2,8), breaks= 30)
cat(
  "number of values loss after take log is:",sum(!is.finite(log(cex5_small$foodaway))), "\n", #計算無效值的個數
  #is.finite()檢查數值是否是有限值；驚嘆號表示邏輯否定 (NOT) -> TRUE變成FALSE(反之亦然)
  "since those sample are 0,and ln(0) is undefined and become missing value",
  sep = ""
)
## number of values loss after take log is:178
## since those sample are 0,and ln(0) is undefined and become missing value

#(d)
idx <- !is.na(cex5_small$foodaway) & cex5_small$foodaway > 0 &
  !is.na(cex5_small$income)
#!is.na(cex5_small$foodaway) -> 檢查 foodaway 是否不是缺失值 (missing value)且! -> NOT,所以有資料 -> TRUE

df <- cex5_small[idx, ]

mod1 <- lm(log(foodaway) ~ income, data = df)
intercept <- coef(mod1)[1]
slope     <- coef(mod1)[2]
cat(
  "intercept is: ",intercept, "\n",
  "slope is: ", slope, "\n",
  "We estimate that each additional $100 household income increases food away expenditures", "\n",
  "per person of about 0.69%, other factors held constant", "\n",
  sep=""
)    
## intercept is: 3.1293
## slope is: 0.006901748
## We estimate that each additional $100 household income increases food away expenditures
## per person of about 0.69%, other factors held constant

#(e)
lnfood = log(cex5_small$foodaway) #建立一個新變數
plot(cex5_small$income, lnfood, 
     xlab="Income", 
     ylab="ln(foodaway)",
     main = "Observations and Log-Linear Fitted line",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept,slope ,col = 'red')
legend("bottomright",legend = c("foodaway", "Fitted line"),
       col    = c("blue", "red"),pch    = c(16, NA),     lty    = c(NA, 1),     
       lwd    = c(NA, 2))
##The plot shows a positive association between ln(FOODAWAY) and INCOMEs. 
#legend("bottomright", -> 設定圖例位置
#legend = c("foodaway", "Fitted line") ->設定兩個圖例文字(藍點 -> 資料點；紅線 -> 迴歸線)
#lty = c(NA, 1) -> 線形，1表示實線 ； lwd -> 線寬

#(f)
res = resid(mod1)
plot(df$income,res,
     xlab = "Income",ylab = "OLS residuals",
     main = "Residual vs. Income",
     ylim = c(-4,4),
     pch =16,col = 'blue',
     type='p')
##The OLS residuals do appear randomly distributed with no obvious patterns. There are fewer observations at higher incomes, so there is more “white space.” 