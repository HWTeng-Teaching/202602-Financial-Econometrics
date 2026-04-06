url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
#load(url(url))
destfile <- tempfile(fileext = ".rdata")
download.file(url, destfile, mode = "wb")
load(destfile)

str(collegetown)
summary(collegetown)

data <- collegetown

mean_price <- mean(data$price)
mean_sqft <- mean(data$sqft)

# (a) log-linear model 
mod_loglinear <- lm(log(price) ~ sqft, data = data)
summary(mod_loglinear)

b2 <- coef(mod_loglinear)[2]
slope_loglinear <- b2 * mean_price
elas_loglinear  <- b2 * mean_sqft

cat('slope:',slope_loglinear,"\n")
cat('elasticity:',elas_loglinear,"\n")

# (b) log-log model
mod_loglog <- lm(log(price) ~ log(sqft), data = data)
summary(mod_loglog)

a2 <- coef(mod_loglog)[2]
slope_loglog <- a2 * (mean_price / mean_sqft)
elas_loglog  <- a2 

cat('slope:',slope_loglog,"\n")
cat('elasticity:',elas_loglog,"\n")

# (c) generalized R squared
mod_linear <- lm(price ~ sqft, data = data)
summary(mod_linear)

sigma2_loglinear <- sum(resid(mod_loglinear)^2) / df.residual(mod_loglinear)
sigma2_loglog <- sum(resid(mod_loglog)^2) / df.residual(mod_loglog)

gr2_loglinear <- cor(data$price, exp(fitted(mod_loglinear) + sigma2_loglinear/2))^2
gr2_loglog <- cor(data$price, exp(fitted(mod_loglog) + sigma2_loglog/2))^2

cat(gr2_loglinear,"\n")
cat(gr2_loglog,"\n")

#(d) Jarque-Bera test
library(tseries)
hist(resid(mod_linear), main = "Linear Model", xlab = "Residual")
jarque.bera.test(resid(mod_linear))

hist(resid(mod_loglinear), main = "Log-linear Model", xlab = "Residual")
jarque.bera.test(resid(mod_loglinear))

hist(resid(mod_loglog), main = "Log-log Model", xlab = "Residual")
jarque.bera.test(resid(mod_loglog))

#(e) residual plot
plot(data$sqft, resid(mod_linear),
     main = "Linear Model",xlab = "SQFT",ylab = "Residuals")
abline(h=0, lty=2)
plot(data$sqft, resid(mod_loglinear),
     main = "Log-linear Model",xlab = "SQFT",ylab = "Residuals") 
abline(h=0, lty=2)
plot(data$sqft, resid(mod_loglog),
     main = "Log-log Model",xlab = "SQFT",ylab = "Residuals") 
abline(h=0, lty=2)

# (f) Prediction at SQFT = 2700 
x0 <- 27
newd <- data.frame(sqft = x0)

predict(mod_linear, newdata = newd)

exp(predict(mod_loglinear, newdata = newd))  # natural
exp(predict(mod_loglinear, newdata = newd) + sigma2_loglinear/2)  # corrected

exp(predict(mod_loglog, newdata = newd))  # natural
exp(predict(mod_loglog, newdata = newd) + sigma2_loglog/2)  # corrected

# (g) 95% PI at SQFT = 2700 
predict(mod_linear, newdata = newd, interval = "prediction", level = 0.95)

exp(predict(mod_loglinear, newdata = newd, interval = "prediction", level = 0.95))

exp(predict(mod_loglog, newdata = newd, interval = "prediction", level = 0.95))