rm(list=ls())

###Problem 2.25

library(devtools)
library(PoEdata)

url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"   
dest <- file.path(tempdir(), "cex5_small.rdata")
download.file(url, dest, mode = "wb") 
load(dest)

##a.
cat(
  "Mean of FOODWAY:", mean(cex5_small$foodaway), "\n",
  "Median of FOODWAY:", median(cex5_small$foodaway), "\n",
  "25th percentiles of FOODWAY:", quantile(cex5_small$foodaway, 0.25), "\n",
  "75th percentiles of FOODWAY:", quantile(cex5_small$foodaway, 0.75), "\n",
  sep = " "
)    

hist(cex5_small$foodaway,xlab = 'foodaway from home expenditure per month per person past quarte', ylab = 'frequency', main = 'Histogram of FOODAWAY',
     xlim = c(0,1000), breaks= 20)

##b.
cat(
  "advanced = 1:", "\n",
  "N:", sum(cex5_small$advanced), "\n",
  "Mean:", mean(cex5_small$foodaway[cex5_small$advanced == 1]), "\n",
  "Median:", median(cex5_small$foodaway[cex5_small$advanced == 1]), "\n",
  "college = 1:", "\n",
  "N:", sum(cex5_small$college), "\n",
  "Mean:", mean(cex5_small$foodaway[cex5_small$college == 1]), "\n",
  "Median:", median(cex5_small$foodaway[cex5_small$college == 1]), "\n",
  "None:", "\n",
  "N:", sum(cex5_small$advanced == 0 & cex5_small$college == 0), "\n",  
  "Mean:", mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0]), "\n",
  "Median:", median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0]), "\n",
  sep = ""
)      

##c.
hist(log(cex5_small$foodaway),prob = TRUE,xlab = 'foodaway', ylab = 'Percent', main = 'Histogram of FOODAWAY',
     xlim = c(-2,8), breaks= 30)

cat(
  "Number of values loss after take log is:",sum(!is.finite(log(cex5_small$foodaway))), "\n",
  "Since those sample are 0, ln(0) is undefined and become missing value",
  sep = ""
)

##d.
idx <- !is.na(cex5_small$foodaway) & cex5_small$foodaway > 0 &
  !is.na(cex5_small$income)

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

##e.
lnfood = log(cex5_small$foodaway)
plot(cex5_small$income, lnfood, 
     xlab="Income", 
     ylab="ln(foodaway)",
     main = "Obs and log-linear fitted line",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept,slope ,col = 'red')
legend("bottomright",legend = c("foodaway", "Fitted line"),
       col    = c("blue", "red"),pch    = c(16, NA),     lty    = c(NA, 1),     
       lwd    = c(NA, 2))         

##f.
res = resid(mod1)
plot(df$income,res,
     xlab = "Income",ylab = "OLS residuals",
     main = "Residual vs Income",
     ylim = c(-4,4),
     pch =16,col = 'blue',
     type='p')