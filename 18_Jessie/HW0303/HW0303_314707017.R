
#2.22(a)
if (!require("devtools")) install.packages("devtools")
devtools::install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
data("star5_small")
data_ab <- subset(star5_small, small == 1 | regular == 1)
model_a <- lm(totalscore ~ small, data = data_ab)
summary(model_a)


#(b)
model_b_read <- lm(readscore ~ small, data = data_ab)
model_b_math <- lm(mathscore ~ small, data = data_ab)
summary(model_b_read)
summary(model_b_math)


#(c)
data_cd <- subset(star5_small, aide == 1 | regular == 1)
model_c <- lm(totalscore ~ aide, data = data_cd)
summary(model_c)


#(d)
model_d_read <- lm(readscore ~ aide, data = data_cd)
model_d_math <- lm(mathscore ~ aide, data = data_cd)
summary(model_d_read)
summary(model_d_math)



#2.25(a)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("cex5_small")

hist(cex5_small$foodaway,
     main = "Histogram of FOODAWAY",
     xlab = " Food Away from Home",
     col = "purple")

summary(cex5_small$foodaway)

mean(cex5_small$foodaway)
median(cex5_small$foodaway)
quantile(cex5_small$foodaway, probs = c(0.25, 0.75))


#(b)
#advanced degree
mean(cex5_small$foodaway[cex5_small$advanced == 1])
median(cex5_small$foodaway[cex5_small$advanced == 1])

#college degree
mean(cex5_small$foodaway[cex5_small$college == 1])
median(cex5_small$foodaway[cex5_small$college == 1])

#no advanced or college degree
mean(cex5_small$foodaway[cex5_small$college == 0 & cex5_small$advanced == 0])
median(cex5_small$foodaway[cex5_small$college == 0 & cex5_small$advanced == 0])


#(c)
cex5_log <- subset(cex5_small, foodaway > 0)
cex5_log$ln_foodaway <- log(cex5_log$foodaway)

hist(cex5_log$ln_foodaway,
     main = "Histogram of ln(FOODAWAY)",
     xlab = "ln(Food Away)",
     col = "blue")

summary(cex5_log$ln_foodaway)


#(d)
cex5_log <- subset(cex5_small, foodaway > 0)
cex5_log$ln_foodaway <- log(cex5_log$foodaway)
model_d <- lm(ln_foodaway ~ income, data = cex5_log)
summary(model_d)


#(e)
plot(cex5_small$income,
     cex5_small$ln_foodaway,
     main = "ln(FOODAWAY) vs INCOME",
     xlab = "Income (in $100)",
     ylab = "ln(Food Away)",
     pch = 20)

abline(model_d, col = "brown", lwd = 2)


#(f)
resid_d <- resid(model_d)
summary(resid_d)

plot(cex5_log$income,
     resid_d,
     main = "Residuals vs Income",
     xlab = "Income (in $100)",
     ylab = "Residuals",
     pch = 20)

abline(h = 0, col = "brown", lwd = 2)


