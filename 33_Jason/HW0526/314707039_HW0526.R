library(tidyverse)
library(sandwich)  # robust standard errors
library(lmtest)  
library(zoo)
library(pROC)
library(PoEdata)
data("lasvegas")
#a小題
lpm <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
          data = lasvegas)
coeftest(lpm, vcov = vcovHC(lpm, type = "HC1"))

#b小題
logit<- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
          data = lasvegas,family=binomial(link="logit"))
summary(logit)

#c小題
obs_500_1000<-lasvegas[c(500,1000),]
pred_lpm<-predict(lpm,newdata = obs_500_1000)
pred_logit <- predict(logit, newdata = obs_500_1000,type = "response")
data.frame(Observation = c(500, 1000),
           Actual      = lasvegas$delinquent[c(500, 1000)],
           LPM_pred    = pred_lpm,
           Logit_pred  = pred_logit)

#d小題
base_case <- data.frame(
  lvr = 80, ref = 0, insur = 0, rate = 8,
  amount = 2.5, term = 30, arm = 0
)
credit_vals <- c(500, 600, 700)
results_d <- lapply(credit_vals, function(cr) {
  df <- base_case; df$credit <- cr
  data.frame(
    credit     = cr,
    LPM_prob   = predict(lpm, newdata = df),
    Logit_prob = predict(logit, newdata = df, type = "response")
  )
})
do.call(rbind, results_d)

hist(lasvegas$credit, main = "Histogram of credit",
     xlab = "credit", col = "steelblue", breaks = 30)

#e小題
lpm_me_credit <- coef(lpm)["credit"]
results_e <- lapply(credit_vals, function(cr) {
  df <- base_case; df$credit <- cr
  p  <- predict(logit, newdata = df, type = "response")
  me <- p * (1 - p) * coef(logit)["credit"]
  data.frame(credit = cr, Logit_ME = me, LPM_ME = lpm_me_credit)
})
do.call(rbind, results_e)

#f小題
hist(lasvegas$lvr, main = "Histogram of lvr",
     xlab = "lvr", col = "coral", breaks = 30)
lvr_vals <- c(20, 80)
results_f <- lapply(lvr_vals, function(lv) {
  df <- base_case; df$lvr <- lv; df$credit <- 600
  data.frame(
    lvr        = lv,
    LPM_prob   = predict(lpm, newdata = df),
    Logit_prob = predict(logit, newdata = df, type = "response")
  )
})
do.call(rbind, results_f)

#g小題
lpm_accuracy   <- mean(ifelse(fitted(lpm) >= 0.5, 1, 0) == lasvegas$delinquent)
logit_accuracy <- mean(ifelse(fitted(logit) >= 0.5, 1, 0) == lasvegas$delinquent)
cat("LPM Accuracy:  ", round(lpm_accuracy, 4), "\n")
cat("Logit Accuracy:", round(logit_accuracy, 4), "\n")

#h小題
train <- lasvegas[1:500, ]
test  <- lasvegas[501:1000, ]
logit_train <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
                   data = train, family = binomial(link = "logit"))
test$pred_prob    <- predict(logit_train, newdata = test, type = "response")
test$pred_class   <- ifelse(test$pred_prob >= 0.5, 1, 0)
acc_05 <- mean(test$pred_class == test$delinquent)
cat("Accuracy (threshold=0.5):", round(acc_05, 4), "\n")

roc_obj <- roc(test$delinquent, test$pred_prob)
plot(roc_obj, main = "ROC Curve")
cat("AUC:", auc(roc_obj), "\n")