rm(list=ls())

# 3.18

# a.
intercept = 6.855
slope = 3.880

mean_income = 59.3

mean_insurance = intercept + slope * mean_income

income_x = seq(0,100, by=1)
insurance_y = intercept + slope * income_x

plot(income_x,insurance_y,type='l', col='blue',lwd = 2,
     main = "Fitted Relationship: Insurance and Income",
     xlab = "Income(thousand of $)",
     ylab = "Life Insurance(thousand of $)",
     xlim = c(0,100), ylim = c(0,400))


points(0, intercept, col = "red", pch = 16, cex = 1.5)
points(mean_income, mean_insurance, col = "darkgreen", pch = 17, cex = 1.5)

# b.
slope_est = 3.880
se_slope = 0.112
N = 20

point_estimate = slope_est

df = N - 2

t_critical = qt(0.975, df=df)
lower_bound = slope_est - t_critical * se_slope
upper_bound = slope_est + t_critical * se_slope


cat("Point Estimate (in dollars): $", point_estimate * 1000, "\n\n")
cat("Degrees of Freedom:", df, "\n")
cat("t-critical value:", round(t_critical, 3), "\n")
cat("95% Interval Estimate (in thousands): [", round(lower_bound, 3), ", ", round(upper_bound, 3), "]\n")
cat("95% Interval Estimate (in dollars): [ $", round(lower_bound * 1000), ", $", round(upper_bound * 1000), "]\n")

#c
b1 = 6.855
b2 = 3.880
se_b1 = 7.383
se_b2 = 0.112
cov = -0.746
x0 = 100

y0_hat = b1 + b2 * x0

var_y0_hat = (se_b1^2) + (x0^2)*(se_b2^2) + (2*x0*cov)
se_y0_hat = sqrt(var_y0_hat)

df = N-2
t = qt(0.995,df)

lower_bound_y0_hat = y0_hat - t*se_y0_hat 
upper_bound_y0_hat = y0_hat + t*se_y0_hat

cat("Point Estimate:", y0_hat, "\n")
cat("Standard Error of Expected Value:", se_y0_hat, "\n")
cat("t-critical (99%, df=18):", t, "\n")
cat("99% Confidence Interval: [", lower_bound_y0_hat, ",", upper_bound_y0_hat, "]\n")


#d
beta2 = 5
alpha = 0.5

t_stat = (b2 - beta2)/se_b2
cat("Test Statistics(t):",t_stat,"\n")

t_critical_point = qt(1-alpha/2,df)
cat("Rejection Region: t <= -", t_critical_point, " or t >= ", t_critical_point, "\n", sep="")

if (abs(t_stat) >= t_critical_point) {
  print("Reject the Null Hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

#e
beta2_2 = 1
t_stat_2 = (b2-beta2_2)/ se_b2
cat("Test Statistics(t):",t_stat_2,"\n")

t_critical_point2 = qt(0.99,df)
cat("Rejection Region: t >= ", round(t_critical_point2,3), "\n")

if (t_stat_2>=t_critical_point2){
  print("Reject the Null Hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}


# 3.23
library(POE5Rdata)
data('collegetown')

# a. 
model = lm(price~I(sqft^2),data=collegetown)

summary(model)

smodel = summary(model)
b2_hat = coef(model)["I(sqft^2)"]
se_b2 = coef(smodel)[2,2]
df = df.residual(model)

t = (41*b2_hat-13)/(41*se_b2)

df = df.residual(model)

t_critical = qt(0.95,df=df)

p_value = 1 - pt(t,df=df)

if (t>t_critical){
  print("Reject the Null Hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

cat("t statistic =", t, "\n")
cat("rejection region: t >", t_critical, "\n")
cat("p-value =", p_value, "\n")


# b. 

t = (81*b2_hat-13)/(81*se_b2)

t_critical = qt(0.95,df=df)

p_value = 1 - pt(t,df=df)

if (t>t_critical){
  print("Reject the Null Hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

cat("t statistic =", t, "\n")
cat("rejection region: t >", t_critical, "\n")
cat("p-value =", p_value, "\n")

#c

b = coef(model)
se_b1 = coef(smodel)[1,2]
se_b2 = coef(smodel)[2,2]
cov = vcov(model)[1,2]
x0 = 20

y0_hat = b[1]+b[2]*(x0^2)

var_y0_hat = (se_b1^2) + (x0^4) * (se_b2^2) + 2 * (x0^2) * cov
se_y0_hat = sqrt(var_y0_hat)
  
t = qt(0.975,df)  

lower_bound_y0_hat = y0_hat - t*se_y0_hat 
upper_bound_y0_hat = y0_hat + t*se_y0_hat



cat("Point Estimate:", y0_hat, "\n")
cat("Standard Error of Expected Value:", se_y0_hat, "\n")
cat("t-critical:", t, "\n")
cat("95% Confidence Interval: [", lower_bound_y0_hat, ",", upper_bound_y0_hat, "]\n")

#速解法
#newdata <- data.frame(sqft = 20)
#predict(model, newdata = newdata, interval = "confidence", level = 0.95)

#d
data = collegetown
house_20 = subset(data,sqft==20)

n_20 = nrow(house_20)

mean_price_20 = mean(house_20$price,na.rm=TRUE)

if (mean_price_20 >= lower_bound_y0_hat && mean_price_20<= upper_bound_y0_hat ) {
  print("compatible")
} else {
  print("not compatible")
}


# 3.31
#a.
library(POE5Rdata)
data('tuna')

summary_stat = function(x) {
  c(
    Mean = mean(x,na.rm=TRUE),
    Min = min(x,na.rm=TRUE),
    Max = max(x,na.rm=TRUE),
    SD = sd(x,na.rm=TRUE)
  )
}

summary_stat(tuna$sal1)
summary_stat(tuna$apr1)

tuna$week = 1:nrow(tuna)
plot(tuna$week,tuna$sal1,type='b',xlab = "week",ylab="sales(1)", main = "SAL1 To Week")
plot(tuna$week,tuna$apr1,type='b',xlab = "week",ylab="apr(1)", main = "APR1 To Week")

#b.

plot(tuna$apr1,tuna$sal1,xlab="ARP1",ylab="SAL1",main="ARP1 VS SAL1")
abline(lm(sal1~apr1,data=tuna),lwd =2)

#c.

tuna$price1 = tuna$apr1*100

model = lm(sal1~price1,data=tuna)

sum_model = summary(model)

b1 = coef(model)[1]
b2 = coef(model)[2]
se_b2 = coef(sum_model)[2,2] 
df = df.residual(model)

point_estimate = b2
cat("point estimate:",point_estimate,"\n")

t = qt(0.975,df)
cat("t-critical value:", round(t, 3), "\n")

lower_bound = point_estimate - t * se_b2
upper_bound = point_estimate + t * se_b2

cat("95% Interval Estimate: [", round(lower_bound, 2), ", ", round(upper_bound, 2), "]\n")


#d.
b = coef(model)
se_b1 = coef(sum_model)[1,2]
se_b2 = coef(sum_model)[2,2]
cov = vcov(model)[1,2]
x0 = 70

#V = vcov(model)
#se_b1 = V[1,1], se_b2 = V[2,2]

y0_hat = b[1]+b[2]*x0

var_y0_hat = (se_b1^2) + (x0^2)*(se_b2^2) + (2*x0*cov)
se_y0_hat = sqrt(var_y0_hat)

t = qt(0.95,df)  

lower_bound_y0_hat = y0_hat - t*se_y0_hat 
upper_bound_y0_hat = y0_hat + t*se_y0_hat

cat("Point Estimate:", y0_hat, "\n")
cat("Standard Error of Expected Value:", se_y0_hat, "\n")
cat("t-critical:", t, "\n")
cat("90% Confidence Interval: [", lower_bound_y0_hat, ",", upper_bound_y0_hat, "]\n")

#e.

mean_price1 = mean(tuna$price1,na.rm=TRUE)
mean_sal1 = mean(tuna$sal1,na.rm=TRUE)

elasticity_hat = b2*(mean_price1/mean_sal1)
se_elasticity = (mean_price1/mean_sal1)*se_b2

t_elas = qt(0.975,df)

lower_bound_y0_hat = elasticity_hat - t_elas*se_elasticity 
upper_bound_y0_hat = elasticity_hat + t_elas*se_elasticity

cat("Estimated elasticity at the means:", round(elasticity_hat, 4), "\n")
cat("SE of elasticity:", round(se_elasticity, 4), "\n")
cat("95% CI for elasticity: [", round(lower_bound_y0_hat, 4), ", ", round(upper_bound_y0_hat, 4), "]\n")

#f.
t_stat = (elasticity_hat-(-3))/se_elasticity
t_elas = qt(0.95,df)
p_value_elas = 2*(1-pt(abs(t_stat),df))


cat("Test statistic:", round(t_stat, 4), "\n")
cat("Rejection region: |t| >", round(t_elas, 4), "\n")
cat("p-value:", round(p_value_elas, 4), "\n")

if (abs(t_stat) > t_elas) {
  cat("Conclusion: Reject H0 at the 10% significance level.\n")
} else {
  cat("Conclusion: Do not reject H0 at the 10% significance level.\n")
}





