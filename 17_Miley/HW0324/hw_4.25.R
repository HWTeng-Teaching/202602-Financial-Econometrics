load("Documents/R/data_needed/poe5rdata/collegetown.rdata")

# part a
mod1 <- lm(log(price)~sqft, data = collegetown)
summary_mod1 <- summary(mod1)
table1 <- data.frame(xtable(summary_mod1))
parta_b1 <- table1["(Intercept)", "Estimate"]
parta_b2 <- table1["sqft", "Estimate"]
# b1 = 4.394 
# b2 = 0.036 -> A unit change in sqft leads to approximately 3.6% change in price.

mean_price <- mean(collegetown$price)
mean_sqft <- mean(collegetown$sqft)
# the slope is beta2*y
m1 <- parta_b2*mean_price
# the elasticity is beta2*x
e1 <- parta_b2*mean_sqft
# m1 = 9.0197
# e1 = 0.9834
#------------------------------------------------------------------------------------

# part b
mod2 <- lm(log(price)~log(sqft), data = collegetown)
summary_mod2 <- summary(mod2)
table2 <- data.frame(xtable(summary_mod2))
partb_b1 <- table2["(Intercept)", "Estimate"]
partb_b2 <- table2["log(sqft)", "Estimate"]
# b1 = 2.0497 
# b2 = 1.0248 -> price is an increasing function of sqft at an increasing rate.

# the slope is beta2*y/x
m2 <- partb_b2*mean_price/mean_sqft
# the elasticity is beta2
e2 <- partb_b2
# m2 = 9.399
# e2 = 1.0248
#------------------------------------------------------------------------------------

# part c
mod3 <- lm(price~sqft, data = collegetown)
summary_mod3 <- summary(mod3)
rsq_c <- summary_mod3$r.squared
# rsq_c = 0.6413

# R^2 of part a: ln(price) = B1 + B2SQFT + e
ln_price_hat <- fitted(mod1)
price_hat <- exp(ln_price_hat)
r_a <- cor(collegetown$price, price_hat)
rsq_a <- r_a^2
# rsq_a = 0.6622

# R^2 of part b: ln(price) = B1 + B2ln(SQFT) + e
ln_price_hat_b <- fitted(mod2)
price_hat_b <- exp(ln_price_hat_b)
r_b <- cor(collegetown$price, price_hat_b)
rsq_b <- r_b^2
# rsq_b = 0.6445
#------------------------------------------------------------------------------------

# part d
ehat_a <- mod1$residuals
ggplot(collegetown, aes(x = ehat_a))+
  geom_histogram(bins = 15, color = "white", fill = "lightpink")+
  labs(title = "Residuals plots of a",
       x = "residuals")

ehat_b <- mod2$residuals
ggplot(collegetown, aes(x = ehat_b))+
  geom_histogram(bins = 15, color = "white", fill = "lightyellow")+
  labs(title = "Residuals plots of b",
       x = "residuals")

ehat_c <- mod3$residuals
ggplot(collegetown, aes(x = ehat_c))+
  geom_histogram(bins = 15, color = "white", fill = "lightblue")+
  labs(title = "Residuals plots of c",
       x = "residuals")

jarque.bera.test(ehat_a)
jarque.bera.test(ehat_b)
jarque.bera.test(ehat_c)
#------------------------------------------------------------------------------------

# part e
ggplot(collegetown, aes(x = sqft, y = ehat_a))+
  geom_point()+
  labs(title = "Scatter plots of sqft and residuals (a)",
       x = "sqft",
       y = "residuals")

ggplot(collegetown, aes(x = sqft, y = ehat_b))+
  geom_point()+
  labs(title = "Scatter plots of sqft and residuals (b)",
       x = "sqft",
       y = "residuals")

ggplot(collegetown, aes(x = sqft, y = ehat_c))+
  geom_point()+
  labs(title = "Scatter plots of sqft and residuals (c)",
       x = "sqft",
       y = "residuals")
#------------------------------------------------------------------------------------

# part f
pred_ln_price_a <- parta_b1 + parta_b2*27
pred_price_a <- exp(pred_ln_price_a)
# pred_price_a = 214.2336

pred_ln_price_b <- partb_b1 + partb_b2*log(27)
pred_price_b <- exp(pred_ln_price_b)
# pred_price_b = 227.5386

summary_mod3 <- summary(mod3)
table3 <- data.frame(xtable(summary_mod3))
partc_b1 <- table3["(Intercept)", "Estimate"]
partc_b2 <- table3["sqft", "Estimate"]
pred_price_c <- partc_b1 + partc_b2*27
# pred_price_c = 246.4557
#------------------------------------------------------------------------------------

# part g
sqftx = data.frame(sqft = 27)
pred_a <- predict(mod1, newdata = sqftx, interval ="prediction", level = 0.95)
real_pred_a <- exp(pred_a)
# model a:[109.7685, 418.1167]

pred_b <- predict(mod2, newdata = sqftx, interval ="prediction", level = 0.95)
real_pred_b <- exp(pred_b)
# model b:[111.1406, 465.8406]

predict(mod3, newdata = sqftx, interval ="prediction", level = 0.95)
# model c:[44.2772, 448.6341]
#------------------------------------------------------------------------------------
