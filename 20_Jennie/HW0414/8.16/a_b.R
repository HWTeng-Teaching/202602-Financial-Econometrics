#a
load("D:/碩一下/計量經濟/作業/HW0414/8.16/vacation.rdata")
model =lm(miles ~ income + age + kids, data = vacation)
summary(model)

confint(model, level = 0.95)

#b
res = residuals(model)
plot(vacation$income, res,
     xlab = "Income",
     ylab = "Residuals",
     main = "Residuals vs Income")
abline(h = 0, col = "red")

plot(vacation$age, res,
     xlab = "Age",
     ylab = "Residuals",
     main = "Residuals vs Age")
abline(h = 0, col = "red")


