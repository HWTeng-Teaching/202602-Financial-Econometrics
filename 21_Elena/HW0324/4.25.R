setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("collegetown.rdata")

# (a)
mean_P <- mean(collegetown$price)
mean_S <- mean(collegetown$sqft)

model1 <- lm(log(price) ~ sqft, data = collegetown)
b2_a <- coef(model1)[[2]]
slope_a <- b2_a * mean_P
elas_a <- b2_a * mean_S

summary(model1)
slope_a
elas_a


# (b)
model2 <- lm(log(price) ~ log(sqft), data = collegetown)
b2_b <- coef(model2)[[2]]
slope_b <- b2_b * (mean_P / mean_S)
elas_b <- b2_b

summary(model2)
slope_b
elas_b


# (c)
model3 <- lm(price ~ sqft, data=collegetown)
rsq_3 <- summary(model3)$r.squared  # Linear Model的R^2

# Log-Linear general R^2
sighat2_1 <- summary(model1)$sigma^2
yhat_1 <- exp(fitted(model1) + sighat2_1/2) # fitted(mod_a) 會直接找出 ln(y) 的預測值 yhat
rg_1 <- cor(collegetown$price, yhat_1)^2

# log-log general R^2
sighat2_2 <- summary(model2)$sigma^2
yhat_2 <- exp(fitted(model2) + sighat2_2/2) # fitted(mod_a) 會直接找出 ln(y) 的預測值 yhat
rg_2 <- cor(collegetown$price, yhat_2)^2

rsq_3
rg_1
rg_2


# (d)
install.packages("tseries")
library(tseries)

ehat1 <- model1$residuals;
ehat2 <- model2$residuals;
ehat3 <- model3$residuals;

hist(ehat1, col= "grey", freq= FALSE, main="", ylab="density", xlab="ehat1")
hist(ehat2, col= "grey", freq= FALSE, main="", ylab="density", xlab="ehat2")
hist(ehat3, col= "grey", freq= FALSE, main="", ylab="density", xlab="ehat3")

jarque.bera.test(ehat1)
jarque.bera.test(ehat2)
jarque.bera.test(ehat3)


# (e)
plot(collegetown$sqft, ehat1, xlab="sqft", ylab="Residuals", main="Log-linear Model Residual Plot")
abline(h=0) 

plot(collegetown$sqft, ehat2, xlab="sqft", ylab="Residuals", main="Log-log Model Residual Plot")
abline(h=0)

plot(collegetown$sqft, ehat3, xlab="sqft", ylab="Residuals", main="Linear Model Residual Plot")
abline(h=0)


# (f)
x_new <- data.frame(sqft = 27)

# Log-Linear 模型校正預測
lnyhat_f1 <- predict(model1, newdata=x_new) # 算出 ln(y) 的預測值
pred_1 <- exp(lnyhat_f1 + sighat2_1/2)   # 取指數還原

# Log-Log 模型校正預測
yhat_f2 <- predict(model2, newdata=x_new)
pred_2 <- exp(yhat_f2 + sighat2_2/2)

# linear model
pred_3 <- predict(model3, newdata=x_new)

pred_1
pred_2
pred_3


# (g)
PI1 <- predict(model1, newdata=x_new, interval="prediction", level=0.95)
PI_1 <- exp(PI1)
PI2 <- predict(model2, newdata=x_new, interval="prediction", level=0.95)
PI_2 <- exp(PI2)
PI_3 <- predict(model3, newdata=x_new, interval="prediction", level=0.95)

PI_1
PI_2
PI_3