#a. Summary statistics
summary(df_cex$foodaway)

# Mean & median
mean_food <- mean(df_cex$foodaway, na.rm = TRUE)
med_food <- median(df_cex$foodaway, na.rm = TRUE)

# 25th & 75th percentiles
q_food <- quantile(df_cex$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

mean_food
med_food
q_food

# Histogram
h <- hist(df_cex$foodaway, breaks = 30, plot = FALSE)
h$counts <- h$counts / sum(h$counts)*100

plot(h,
     main = "Histogram of FOODAWAY",
     xlab = "food away from home expenditure per month per person past quarter, $",
     ylab = "Percent")

#b.three groups
g_adv  <- df_cex$advanced == 1
g_col  <- df_cex$advanced == 0 & df_cex$college == 1
g_none <- df_cex$advanced == 0 & df_cex$college == 0

# mean/median
b_adv_mean <- mean(df_cex$foodaway[g_adv],  na.rm = TRUE)
b_adv_med  <- median(df_cex$foodaway[g_adv], na.rm = TRUE)

b_col_mean <- mean(df_cex$foodaway[g_col],  na.rm = TRUE)
b_col_med  <- median(df_cex$foodaway[g_col], na.rm = TRUE)

b_none_mean <- mean(df_cex$foodaway[g_none],  na.rm = TRUE)
b_none_med  <- median(df_cex$foodaway[g_none], na.rm = TRUE)

c(adv_mean=b_adv_mean, adv_median=b_adv_med,
  college_mean=b_col_mean, college_median=b_col_med,
  none_mean=b_none_mean, none_median=b_none_med)

#c.ln(foodaway)
df_cex$ln_foodaway <- ifelse(df_cex$foodaway > 0, log(df_cex$foodaway), NA)
h <- hist(df_cex$ln_foodaway, breaks = 30,plot = FALSE)
h$counts <- h$counts / sum(h$counts) * 100

plot(h,
     main = "Histogram of ln(FOODAWAY)",
     xlab = "ln(foodaway)",
     ylab = "Percent",
     xlim = c(0,8))

summary(df_cex$ln_foodaway)

#d 
mod_d <- lm(ln_foodaway ~ income, data = df_cex)
b1 <- coef(mod_d)[[1]]
b2 <- coef(mod_d)[[2]]
summary(mod_d)


#e
plot(df_cex$income, df_cex$ln_foodaway,
     cex = 0.7, col = "navy", pch = 16,
     xlab = "Income ",
     ylab = "ln(foodaway)",
     main = "log-linear fitted line",
     ylim = c(0,8))

abline(mod_d,col="red",lwd = 2)

#f residual ehat = y - yhat
ok <- is.finite(df_cex$ln_foodaway) & !is.na(df_cex$income)

mod_f <- lm(ln_foodaway ~ income, data = df_cex)
ehat <- resid(mod_f)

plot(df_cex$income[ok], ehat,
     cex = 0.7, col = "navy",pch = 16,
     xlab = "Income ",
     ylab = "OLS residuals",
     main = "Residuals vs Income",
     ylim = c(-4, 4))



