#22
head(star5_small)
names(star5_small)
names(star5)
names(star5_small2)

#a
#假設aide變數=1為有助教，0沒有
data_a <- subset(star5_small, aide==0) 
model_a <- lm(totalscore ~ small, data = data_a)
summary(model_a)

#b
model_b <- lm(readscore ~ small, data = data_a)
model_c <- lm(mathscore ~ small, data = data_a)
summary(model_b)
summary(model_c)

#c
#假設small變數=1為小班，0不是小班
data_b <- subset(star5_small, small==0) 
model_d <- lm(totalscore ~ aide, data = data_b)
summary(model_d)

#d
model_e <- lm(readscore ~ aide, data = data_b)
model_f <- lm(mathscore ~ aide, data = data_b)
summary(model_e)
summary(model_f)


#25
names(cex5_small)

#a
hist(cex5_small$foodaway,
    main = "histogram of foodaway",
    xlab = "food away from home expenditure per month per person past quarter, $")
summary(cex5_small$foodaway)
quantile(cex5_small$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

#b
mean_adv <- mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)
med_adv <- median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)

mean_col <- mean(cex5_small$foodaway[cex5_small$college == 1 ], na.rm = TRUE)
med_col <- median(cex5_small$foodaway[cex5_small$college == 1 ], na.rm = TRUE)

mean_none <- mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0 ], na.rm = TRUE)
med_none <- median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)

#c
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
# 將 -Inf 替換為 NA，以免影響後續計算與繪圖
cex5_small$ln_foodaway[is.infinite(cex5_small$ln_foodaway)] <- NA
hist(cex5_small$ln_foodaway, 
     main = "Histogram of ln(foodaway)", 
     xlab = "ln(foodaway)")
summary(cex5_small$ln_foodaway)

#d
model_1 <- lm(ln_foodaway ~ income, data = cex5_small)
summary(model_1)

#e
plot(cex5_small$income, cex5_small$ln_foodaway,
     xlab="income",
     ylab="ln(foodaway)")
abline(model_1, col ="red")

#f
residuals <- resid(model_1) 
# 取得模型實際使用的 INCOME 資料 (排除掉因 NA 被剔除的列)
income_used <- model_1$model$income
plot(income_used, residuals, 
     main = "Residuals vs Income", 
     xlab = "Income", 
     ylab = "Residuals")
