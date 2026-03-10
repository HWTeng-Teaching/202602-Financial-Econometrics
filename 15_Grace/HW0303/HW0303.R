# 2.22----------------
library(readxl)
star5_small <- read_excel("C:/finance ecomatrics/data/star5_small.xlsx")
head(star5_small)
summary(star5_small)

# (a.) ----- small vs  regular  -----
data_a <- subset(star5_small, small==1 | regular==1)
model_a <- lm(totalscore ~ small, data = data_a)
summary(model_a)

# (b.) ----- small read & math score  -----
model_b_read <- lm(readscore ~ small, data = data_a)
summary(model_b_read)

model_b_math <- lm(mathscore ~ small, data = data_a)
summary(model_b_math)

# (c.) ----- regular vs  aide  -----
data_c <- subset(star5_small, aide==1 | regular==1)
model_c <- lm(totalscore ~ aide, data = data_c)
summary(model_c)

# (d.) -----readscore & mathscore-----
model_d_read <- lm(readscore ~ aide, data = data_c)
summary(model_d_read)

model_d_math <- lm(mathscore ~ aide, data = data_c)
summary(model_d_math)

# 2.25 ----------------------------------
library(readxl)
cex5_small <- read_excel("C:/finance ecomatrics/data/cex5_small.xlsx")
hist(cex5_small$foodaway)
summary(cex5_small$foodaway)

# (b.) ----- 
d1 <- subset(cex5_small, advanced == 1)
mean(d1$foodaway)
median(d1$foodaway)

d2 <- subset(cex5_small, college == 1)
mean(d2$foodaway)
median(d2$foodaway)

d3 <- subset(cex5_small, advanced == 0 & college == 0)
mean(d3$foodaway)
median(d3$foodaway)

# (c.) -----
lnfoodaway <- log(cex5_small$foodaway)
lnfoodaway[is.infinite(lnfoodaway)] <- NA
sum(is.na(lnfoodaway))
hist(lnfoodaway)
summary(lnfoodaway)

# (d.) -----

cex5_small$lnfoodaway <- log(cex5_small$foodaway)
cex5_small$lnfoodaway[is.infinite(cex5_small$lnfoodaway)] <- NA
regmod <- lm(lnfoodaway ~ income, data = cex5_small, , na.action = na.exclude)
summary(regmod)

# (e.) -----
plot(cex5_small$income, cex5_small$lnfoodaway, xlab = "Income",ylab = "ln(foodaway)",main = "Scatterplot + Fitted Line")
abline(regmod)

# (f.) -----
plot(resid(regmod) ~income, data = cex5_small)