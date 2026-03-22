library(POE5Rdata)
data("collegetown")
?data("collegetown")

#(a)
#estimated model
model = lm(price ~ I(sqft^2), data = collegetown)
summary(model)

# marginal effect and std
alpha2 = coef(model)[[2]]
marginal_effect = 2*alpha2*20
sd = 40*(0.005256)

# test
t = (marginal_effect-13)/sd
df = dim(collegetown)[1]-2
p = 1 - pt(q = t, df = df, lower.tail = T)
print(paste("t-value =", t))
print(paste("df =", df))
print(paste("p-value =", p))

#(b)
price <- collegetown$price
sqft <- collegetown$sqft
qrmodel <- lm(price~I(sqft^2))
qrmodel_A1 <- coef(qrmodel)[1]
qrmodel_A2 <- coef(qrmodel)[2]
qrsummary <- summary(qrmodel)
qrmodel_A2_se <- qrsummary$coef[4]
df <- qrsummary$df[2]

# test the marginal effect
sqft_2 <- 40
marginal_effect_2 <- 2*qrmodel_A2*sqft_2
se <- (qrmodel_A2_se*2*sqft_2)
t_statistic_2 <- (marginal_effect_2 - 13)/se
critical_2 <- qt(0.95, df)
pvalue_2 <- 1-pt(t_statistic_2,df)
print(paste("marginal effect =", marginal_effect_2))
print(paste("se =", se))
print(paste("t-statistic =", t_statistic_2))
print(paste("critical value =", critical_2))
print(paste("p-value =", pvalue_2))

#(c)
sqft <- 20
alpha <- 0.05
tc <- qt(1-alpha/2,df)
alpha1 <- model$coef[1]
alpha2 <- model$coef[2]
vara1 <- vcov(model)[1, 1]
vara2 <- vcov(model)[2, 2]
cova1a2 <- vcov(model)[1, 2]
varL = vara1 + (sqft^2)^2*vara2 + 2*sqft^2*cova1a2 # var(L)
seL <- sqrt(varL)
exp_price <- alpha1 + alpha2*sqft^2
print(paste("expected price =", exp_price))

# Interval estimate
lowb <- exp_price-tc*seL
upb <- exp_price+tc*seL
interval<-c(lowb,upb)
interval

#(d)
price<- collegetown$price
str(subset(collegetown,sqft == 20)$price)
summary(subset(collegetown,sqft == 20)$price)