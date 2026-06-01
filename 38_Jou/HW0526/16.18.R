library(lmtest)
library(sandwich)
library(PoEdata)
data("lasvegas")

# a.LPM
lpm <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm, data = lasvegas)

# White heteroskedasticity robust standard errors
coeftest(lpm, vcov. = vcovHC(lpm, type = "HC1"))

# b. Logit model
logit <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
             data = lasvegas, family = binomial(link = "logit"))

summary(logit)

# c. Predicted value for 500th and 1000th observations
pred_lpm_c <- predict(lpm, newdata = lasvegas[c(500, 1000), ])

pred_logit_c <- predict(logit,
                        newdata = lasvegas[c(500, 1000), ],
                        type = "response")

pred_lpm_c
pred_logit_c


# d. Histogram of CREDIT and predicted probabilities
hist(lasvegas$credit,  main = "Histogram of CREDIT", xlab = "Credit score", breaks = 14)

new_credit <- data.frame(lvr = 80, ref = 0, insur = 0, rate = 8, amount = 2.5,
  credit = c(500, 600, 700), term = 30, arm = 0)

prob_lpm_credit <- predict(lpm, newdata = new_credit)
prob_logit_credit <- predict(logit, newdata = new_credit, type = "response")

result_d <- data.frame(credit = c(500, 600, 700), LPM = prob_lpm_credit, Logit = prob_logit_credit)

result_d

# e. Marginal effect of CREDIT
me_lpm_credit <- coef(lpm)["credit"]

# Logit marginal effect = beta_credit * p * (1-p)
beta_credit_logit <- coef(logit)["credit"]

me_logit_credit <- beta_credit_logit * prob_logit_credit * (1 - prob_logit_credit)

result_e <- data.frame(credit = c(500, 600, 700), LPM_ME = rep(me_lpm_credit, 3), Logit_ME = me_logit_credit)

result_e


# f. Histogram of LVR and predicted probabilities when LVR = 20 and 80
hist(lasvegas$lvr, main = "Histogram of LVR", xlab = "Loan-to-value ratio", breaks = 14)

new_lvr <- data.frame(lvr = c(20, 80), ref = 0, insur = 0, rate = 8,
  amount = 2.5, credit = 600, term = 30, arm = 0)

prob_lpm_lvr <- predict(lpm, newdata = new_lvr)
prob_logit_lvr <- predict(logit, newdata = new_lvr, type = "response")

result_f <- data.frame(lvr = c(20, 80), LPM = prob_lpm_lvr, Logit = prob_logit_lvr)

result_f

# g. Correct prediction rate using 0.5 threshold
pred_class_lpm <- ifelse(predict(lpm) > 0.5, 1, 0)

pred_class_logit <- ifelse(predict(logit, type = "response") > 0.5, 1, 0)

correct_lpm <- mean(pred_class_lpm == lasvegas$delinquent)
correct_logit <- mean(pred_class_logit == lasvegas$delinquent)

correct_lpm
correct_logit

table(Predicted = pred_class_lpm, Actual = lasvegas$delinquent)
table(Predicted = pred_class_logit, Actual = lasvegas$delinquent)


# h. Train on first 500, test on second 500
train <- lasvegas[1:500, ]
test  <- lasvegas[501:1000, ]

logit_train <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
                   data = train, family = binomial(link = "logit"))

test_prob <- predict(logit_train, newdata = test, type = "response")

# 0.5 threshold
test_pred_05 <- ifelse(test_prob > 0.5, 1, 0)
mean(test_pred_05 == test$delinquent)

# Try different thresholds
thresholds <- seq(0.05, 0.95, by = 0.05)

accuracy_by_threshold <- sapply(thresholds, function(cutoff) {
  pred <- ifelse(test_prob > cutoff, 1, 0)
  mean(pred == test$delinquent)})

data.frame(threshold = thresholds, accuracy = accuracy_by_threshold)

# Find threshold with highest accuracy
thresholds[which.max(accuracy_by_threshold)]
max(accuracy_by_threshold)

