setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("cex5_small.rdata")

# (a)
hist(cex5_small$foodaway, main="Histogram of FOODAWAY", xlab="Food Away Expenditure")
summary(cex5_small$foodaway)

# (b)
aggregate(foodaway ~ college, data = cex5_small, mean)
aggregate(foodaway ~ college, data = cex5_small, median)

aggregate(foodaway ~ advanced, data = cex5_small, mean)
aggregate(foodaway ~ advanced, data = cex5_small, median)

no_degree_data <- subset(cex5_small, advanced == 0 & college == 0)
summary(no_degree_data$foodaway)

# (c)
hist(log(cex5_small$foodaway), main="Histogram of ln(foodaway)", col="lightgreen")
summary(log(cex5_small$foodaway)) 
sum(cex5_small$foodaway == 0) # 被剔除掉的人數 (即等於 0 的人數)
sum(cex5_small$foodaway > 0) # 取對數後「剩下」的有效樣本數 (即大於 0 的人數)
# (d)
cex_pos <- subset(cex5_small, foodaway > 0)
model_225 <- lm(log(foodaway) ~ income, data = cex_pos)
summary(model_225)

# (e)
plot(log(foodaway) ~ income, data = cex_pos)
abline(model_225, col="red")

#(f)
plot(resid(model_225) ~ income, data = cex_pos)
abline(h=0, col="blue")

