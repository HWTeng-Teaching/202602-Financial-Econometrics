rm(list=ls())
library(lmtest)
library(sandwich)

# to https://github.com/ccolonescu/PoEdata/blob/master/data/lasvegas.rda download data

load("C:/Users/CDH/Downloads/lasvegas.rda")
df = lasvegas

#16.18(a)
lpm <- lm(delinquent ~ lvr + ref + insur + rate +
            amount + credit + term + arm,
          data = df)

# White robust SE
coeftest(lpm, vcov = vcovHC(lpm, type = "HC1"))

#16.18(b)
logit <- glm(delinquent ~ lvr + ref + insur + rate +
               amount + credit + term + arm,
             data = df,
             family = binomial(link = "logit"))

summary(logit)

# robust SE
coeftest(logit, vcov = vcovHC(logit, type = "HC1"))

#16.18(c)
obs <- df[c(500, 1000), ]

# LPM prediction
pred_lpm <- predict(lpm,
                    newdata = obs)

# Logit prediction
pred_logit <- predict(logit,
                      newdata = obs,
                      type = "response")

cbind(obs,
      pred_lpm,
      pred_logit)

#16.18(d)
hist(df$credit,
     main = "Histogram of CREDIT",
     xlab = "credit",
     breaks = 30)

new_credit <- data.frame(
  lvr = 80,
  ref = 0,
  insur = 0,
  rate = 8,
  amount = 2.5,
  credit = c(500, 600, 700),
  term = 30,
  arm = 0
)

# LPM
prob_lpm_credit <- predict(lpm,
                           newdata = new_credit)

# Logit
prob_logit_credit <- predict(logit,
                             newdata = new_credit,
                             type = "response")

cbind(new_credit,
      prob_lpm_credit,
      prob_logit_credit)


#16.18(e)
# LPM marginal effect
me_lpm_credit <- coef(lpm)["credit"]

# Logit marginal effect
p_credit <- predict(logit,
                    newdata = new_credit,
                    type = "response")

me_logit_credit <- coef(logit)["credit"] *
  p_credit *
  (1 - p_credit)

data.frame(
  credit = new_credit$credit,
  me_lpm = me_lpm_credit,
  me_logit = me_logit_credit
)

#16.18(f)
hist(df$lvr,
     main = "Histogram of LVR",
     xlab = "lvr",
     breaks = 30)

new_lvr <- data.frame(
  lvr = c(20, 80),
  ref = 0,
  insur = 0,
  rate = 8,
  amount = 2.5,
  credit = 600,
  term = 30,
  arm = 0
)

# LPM
prob_lpm_lvr <- predict(lpm,
                        newdata = new_lvr)

# Logit
prob_logit_lvr <- predict(logit,
                          newdata = new_lvr,
                          type = "response")

cbind(new_lvr,
      prob_lpm_lvr,
      prob_logit_lvr)

#16.18(g)
# 預測值
p_lpm_all <- predict(lpm)

p_logit_all <- predict(logit,
                       type = "response")

# classification
yhat_lpm <- ifelse(p_lpm_all >= 0.5, 1, 0)

yhat_logit <- ifelse(p_logit_all >= 0.5, 1, 0)

# accuracy
acc_lpm <- mean(yhat_lpm == df$delinquent)

acc_logit <- mean(yhat_logit == df$delinquent)

acc_lpm
acc_logit

# confusion matrix
table(actual = df$delinquent,
      pred_lpm = yhat_lpm)

table(actual = df$delinquent,
      pred_logit = yhat_logit)

#16.18(h)
# training data
train <- df[1:500, ]

# testing data
test <- df[501:1000, ]

# train logit
logit_train <- glm(delinquent ~ lvr + ref + insur + rate +
                     amount + credit + term + arm,
                   data = train,
                   family = binomial(link = "logit"))

# prediction on test data
p_test <- predict(logit_train,
                  newdata = test,
                  type = "response")

##################################################
# threshold = 0.5
##################################################

approve_05 <- ifelse(p_test < 0.5, 1, 0)

repay <- ifelse(test$delinquent == 0, 1, 0)

acc_05 <- mean(approve_05 == repay)

acc_05

##################################################
# search for best threshold
##################################################

thresholds <- seq(0.01, 0.99, by = 0.01)

acc <- sapply(thresholds, function(cut){
  
  approve <- ifelse(p_test < cut, 1, 0)
  
  mean(approve == repay)
  
})

best_cut <- thresholds[which.max(acc)]

best_acc <- max(acc)

best_cut
best_acc

# plot
plot(thresholds,
     acc,
     type = "l",
     xlab = "threshold",
     ylab = "accuracy",
     main = "Threshold Selection")

abline(v = best_cut, lty = 2)