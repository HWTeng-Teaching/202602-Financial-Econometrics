# Homework 9 - Chapter 16 Models with Limited Dependent Variables
# Exercises 16.1, 16.4, and 16.18

library(POE5Rdata)
library(lmtest)
library(sandwich)
library(pROC) # for prediction accuracy if needed
library(dplyr)

# =========================================================
# Exercise 16.1
# =========================================================
cat("--- Exercise 16.1 ---\n")

# Logit coefficients from problem
gamma1_logit <- -0.2376
gamma2_logit <- 0.5311

# LPM and Probit coefficients estimated from 'transport' data
data(transport)
lpm_trans <- lm(auto ~ dtime, data=transport)
b1_lpm <- coef(lpm_trans)[1]
b2_lpm <- coef(lpm_trans)[2]

probit_trans <- glm(auto ~ dtime, family=binomial(link="probit"), data=transport)
b1_probit <- coef(probit_trans)[1]
b2_probit <- coef(probit_trans)[2]

# a. Logit P(AUTO=1 | DTIME=1)
z_logit_a <- gamma1_logit + gamma2_logit * 1
p_logit_a <- exp(z_logit_a) / (1 + exp(z_logit_a))
cat("16.1a: Logit Probability (DTIME=1): ", p_logit_a, "\n")

# b. Probit P(AUTO=1 | DTIME=1)
z_probit_b <- b1_probit + b2_probit * 1
p_probit_b <- pnorm(z_probit_b)
cat("16.1b: Probit Probability (DTIME=1): ", p_probit_b, "\n")

# c. Marginal effect of 10 min increase in travel time at DTIME=3 (30 min longer by bus)
# DTIME = (Bus - Auto)/10, so 10 min increase in Bus means Delta DTIME = 1.
# Marginal effect per 1 unit change (10 min)
z_logit_c <- gamma1_logit + gamma2_logit * 3
p_logit_c <- exp(z_logit_c) / (1 + exp(z_logit_c))
me_logit_c <- gamma2_logit * p_logit_c * (1 - p_logit_c)
me_lpm_c <- b2_lpm

cat("16.1c: Marginal Effect (DTIME=3):\n")
cat("Logit ME: ", me_logit_c, "\n")
cat("LPM ME: ", me_lpm_c, "\n")

# d. Marginal effect of 10 min decrease in travel time at DTIME=-5 (50 min longer by driving)
# Decrease in travel time (10 min) means Delta DTIME = -1? 
# Wait, "decrease in travel time of 10 minutes" usually refers to the effect of the variable.
# Marginal effect is dP/dX.
z_logit_d <- gamma1_logit + gamma2_logit * (-5)
p_logit_d <- exp(z_logit_d) / (1 + exp(z_logit_d))
me_logit_d <- gamma2_logit * p_logit_d * (1 - p_logit_d)

z_probit_d <- b1_probit + b2_probit * (-5)
me_probit_d <- b2_probit * dnorm(z_probit_d)

cat("16.1d: Marginal Effect (DTIME=-5):\n")
cat("Logit ME: ", me_logit_d, "\n")
cat("Probit ME: ", me_probit_d, "\n\n")

# =========================================================
# Exercise 16.4
# =========================================================
cat("--- Exercise 16.4 ---\n")
# Logit: P(y=1) = Lambda(-1.836 + 3.021x)
g1_4 <- -1.836
g2_4 <- 3.021

# a. P(y=1 | x=1.5)
z_4a <- g1_4 + g2_4 * 1.5
p_4a <- exp(z_4a) / (1 + exp(z_4a))
cat("16.4a: P(y=1 | x=1.5): ", p_4a, "\n")

# b. Predict y (threshold 0.5)
pred_y_4b <- if(p_4a > 0.5) 1 else 0
cat("16.4b: Predicted y: ", pred_y_4b, " (Actual y=1)\n")

# c. Likelihood for g1=-1, g2=2 with data (1.5, 1), (1.0, 0), (0.5, 0)
g1_c <- -1
g2_c <- 2
x <- c(1.5, 1.0, 0.5)
y <- c(1, 0, 0)

z_c <- g1_c + g2_c * x
p_c <- exp(z_c) / (1 + exp(z_c))
lik_c <- prod(p_c^y * (1-p_c)^(1-y))

lik_max <- exp(-1.612)
cat("16.4c: Likelihood (g1=-1, g2=2): ", lik_c, "\n")
cat("Max Likelihood (e^-1.612): ", lik_max, "\n")
cat("Is max larger? ", lik_max > lik_c, "\n\n")

# =========================================================
# Exercise 16.18
# =========================================================
cat("--- Exercise 16.18 ---\n")
load("lasvegas.rda")

# a. LPM with White Robust SE
mod_lpm <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, data=lasvegas)
cat("16.18a: LPM Summary (Robust SE)\n")
print(coeftest(mod_lpm, vcov = vcovHC(mod_lpm, type = "HC1")))

# b. Logit
mod_logit <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, 
                 family=binomial(link="logit"), data=lasvegas)
cat("\n16.18b: Logit Summary\n")
print(summary(mod_logit))

# c. Predicted value for 500th and 1000th
obs_500_1000 <- lasvegas[c(500, 1000), ]
pred_lpm <- predict(mod_lpm, newdata=obs_500_1000)
pred_logit <- predict(mod_logit, newdata=obs_500_1000, type="response")

cat("\n16.18c: Predictions\n")
cat("500th Obs - LPM: ", pred_lpm[1], " Logit: ", pred_logit[1], "\n")
cat("1000th Obs - LPM: ", pred_lpm[2], " Logit: ", pred_logit[2], "\n")

# d. Prob for CREDIT=500, 600, 700
# Scen: LVR=80, RATE=8, AMOUNT=2.5, Indicators=0, TERM=30
scen_d <- data.frame(lvr=80, ref=0, insur=0, rate=8, amount=2.5, credit=c(500, 600, 700), term=30, arm=0)
p_d_lpm <- predict(mod_lpm, newdata=scen_d)
p_d_logit <- predict(mod_logit, newdata=scen_d, type="response")

cat("\n16.18d: Probabilities for Credit Scores\n")
print(data.frame(Credit=c(500, 600, 700), LPM=p_d_lpm, Logit=p_d_logit))

# e. Marginal effect of CREDIT at 500, 600, 700
me_credit_lpm <- coef(mod_lpm)["credit"]
# Logit ME = beta_credit * P * (1-P)
me_credit_logit <- coef(mod_logit)["credit"] * p_d_logit * (1 - p_d_logit)

cat("\n16.18e: Marginal Effects of CREDIT\n")
print(data.frame(Credit=c(500, 600, 700), LPM_ME=me_credit_lpm, Logit_ME=me_credit_logit))

# f. Prob for LVR=20, 80 (CREDIT=600, others as in d)
scen_f <- data.frame(lvr=c(20, 80), ref=0, insur=0, rate=8, amount=2.5, credit=600, term=30, arm=0)
p_f_lpm <- predict(mod_lpm, newdata=scen_f)
p_f_logit <- predict(mod_logit, newdata=scen_f, type="response")

cat("\n16.18f: Probabilities for LVR\n")
print(data.frame(LVR=c(20, 80), LPM=p_f_lpm, Logit=p_f_logit))

# g. Percentage correct predictions (threshold 0.5)
pred_all_lpm <- ifelse(predict(mod_lpm) > 0.5, 1, 0)
pred_all_logit <- ifelse(predict(mod_logit, type="response") > 0.5, 1, 0)

acc_lpm <- mean(pred_all_lpm == lasvegas$delinquent)
acc_logit <- mean(pred_all_logit == lasvegas$delinquent)

cat("\n16.18g: Accuracy (Threshold 0.5)\n")
cat("LPM Accuracy: ", acc_lpm, "\n")
cat("Logit Accuracy: ", acc_logit, "\n")

# h. Best decision rule
# Train on first 500, test on last 500
train <- lasvegas[1:500, ]
test <- lasvegas[501:1000, ]
mod_train <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, 
                 family=binomial(link="logit"), data=train)
pred_test <- predict(mod_train, newdata=test, type="response")

# Accuracy for different thresholds
thresholds <- seq(0.1, 0.9, by=0.1)
acc_test <- sapply(thresholds, function(t) mean(ifelse(pred_test > t, 1, 0) == test$delinquent))
cat("\n16.18h: Test Accuracy on observations 501-1000 (Training on 1-500)\n")
print(data.frame(Threshold=thresholds, Accuracy=acc_test))
