load("cex5_small.rdata")
#(a)
hist(cex5_small$foodaway)
cat("Summary Statistics of Foodaway: \n")
print(summary(cex5_small$foodaway))

#(b)
adv_foodaway = subset(cex5_small,advanced == 1)
col_foodaway = subset(cex5_small,college == 1)
non_foodaway = subset(cex5_small,college == 0 & advanced == 0)
cat("Summary Statistics of Foodaway with Advanced Degree: \n")
print(summary(adv_foodaway$foodaway))
cat("Summary Statistics of Foodaway with College Degree: \n")
print(summary(col_foodaway$foodaway))
cat("Summary Statistics of Foodaway without Advanced and College Degree: \n")
print(summary(non_foodaway$foodaway))

cex5_pos <- subset(cex5_small, foodaway > 0)
cat("Number of observation in foodaway: ",nrow(cex5_small),end="\n")
cat("Number of observation in ln(foodaway): ",nrow(cex5_pos),end="\n")
#(c)
cat("Summary Statistics of Foodaway with logrithm of y: \n")
hist(log(cex5_pos$foodaway))
print(summary(log(cex5_pos$foodaway)))

#(d)
mod1 <- lm(log(foodaway) ~ income, data=cex5_pos)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat("ln(FoodAway) =",b1,"+",b2,"income + e_i\n" )

#(e)
plot(cex5_pos$income,log(cex5_pos$foodaway))
abline(b1,b2)

#(f)
r = resid(mod1)
plot(cex5_pos$income, r)