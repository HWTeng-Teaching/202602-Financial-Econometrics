library(sandwich)
library(lmtest)
load("lasvegas.rda")
#(a)
cat("(a)\n")
lpm <- lm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
          data = lasvegas)
print(summary(lpm))
print(coeftest(lpm, vcov = vcovHC(lpm, type = "HC1")))

#(b)
cat("(b)\n")
logit_model <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
                   data = lasvegas,family = binomial(link = "logit"))
print(summary(logit_model))

#(c)
cat("(c)\n")
cat("Linear Probability Prediction:\n")
cat("500th observation: ",predict(lpm, newdata = lasvegas[500, ]),"\n")
cat("1000th observation: ",predict(lpm, newdata = lasvegas[1000, ]),"\n")

cat("Logit Prediction:\n")
cat("500th observation: ",
    predict(logit_model,newdata = lasvegas[500, ],type = "response"),"\n")
cat("1000th observation: ",
    predict(logit_model,newdata = lasvegas[1000, ],type = "response"),"\n")

#(d)
cat("(d)\n")
hist(lasvegas$credit,main = "Histogram of CREDIT",xlab = "Credit Score")
abline(v = 500, lty = 2)
abline(v = 600, lty = 2)
abline(v = 700, lty = 2)

borrower1 <- data.frame(lvr = 80, ref = 0,insur = 0,rate = 8,
                        amount = 2.5, credit = 500, term = 30,arm = 0)
borrower2 <- data.frame(lvr = 80, ref = 0,insur = 0,rate = 8,
                        amount = 2.5, credit = 600, term = 30,arm = 0)
borrower3 <- data.frame(lvr = 80, ref = 0,insur = 0,rate = 8,
                        amount = 2.5, credit = 700, term = 30,arm = 0)

cat("Linear Probability Prediction:\n")
cat("Credit=500:",predict(lpm, newdata = borrower1),"\n")
cat("Credit=600:",predict(lpm, newdata = borrower2),"\n")
cat("Credit=700:",predict(lpm, newdata = borrower3),"\n")

cat("Logit Prediction:\n")
cat("Credit=500:",predict(logit_model, newdata = borrower1,type = "response"),"\n")
cat("Credit=600:",predict(logit_model, newdata = borrower2,type = "response"),"\n")
cat("Credit=700:",predict(logit_model, newdata = borrower3,type = "response"),"\n")

#(e)
cat("(e)\n")
cat("Linear Probability Model:\n")
beta_credit_lpm <- coef(lpm)["credit"]
cat("Marginal effect with Credit=500:",beta_credit_lpm, "\n")
cat("Marginal effect with Credit=600:",beta_credit_lpm, "\n")
cat("Marginal effect with Credit=700:",beta_credit_lpm, "\n")

cat("Logit Model:\n")
beta_credit <- coef(logit_model)["credit"]
p1 <- predict(logit_model,newdata = borrower1,type = "response")
ME1 <-  beta_credit * p1 * (1-p1)
cat("Marginal effect with Credit=500:",ME1,"\n")
p2 <- predict(logit_model,newdata = borrower2,type = "response")
ME2 <-  beta_credit * p2 * (1-p2)
cat("Marginal effect with Credit=600:",ME2,"\n")
p3 <- predict(logit_model,newdata = borrower3,type = "response")
ME3 <-  beta_credit * p3 * (1-p3)
cat("Marginal effect with Credit=700:",ME3,"\n")



#(f)
cat("(f)\n")
hist(lasvegas$lvr,main = "Histogram of LVR",xlab = "Ratio of loan amount to value")
dataf1 <- data.frame(lvr = 20, ref = 0,insur = 0,rate = 8,
                        amount = 2.5, credit = 600, term = 30,arm = 0)
dataf2 <- data.frame(lvr = 80, ref = 0,insur = 0,rate = 8,
                        amount = 2.5, credit = 600, term = 30,arm = 0)
cat("Linear Probability Prediction:\n")
cat("LVR=20:",predict(lpm, newdata = dataf1),"\n")
cat("LVR=80:",predict(lpm, newdata = dataf2),"\n")

cat("Logit Prediction:\n")
cat("LVR=20:",predict(logit_model, newdata = dataf1,type = "response"),"\n")
cat("LVR=80:",predict(logit_model, newdata = dataf2,type = "response"),"\n")

#(g)
cat("(g)\n")
cat("Linear Probability Prediction:\n")
lpm_prob <- predict(lpm)
lpm_pred <- ifelse(lpm_prob > 0.5, 1, 0)
cat("Linear Probability Prediction:",mean(lpm_pred == lasvegas$delinquent),"\n")

cat("Logit Prediction:\n")
logit_prob <- predict(logit_model,type = "response")
logit_pred <- ifelse(logit_prob > 0.5, 1, 0)
cat("Logit Prediction:",mean(logit_pred == lasvegas$delinquent),"\n")

print(table(True = lasvegas$delinquent,Predicted = lpm_pred))
print(table(True = lasvegas$delinquent,Predicted = logit_pred))

#(h)
cat("(h)\n")
train <- lasvegas[1:500, ]
test <- lasvegas[501:1000, ]

logit_train <- glm(delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm,
                   data = train,family = binomial(link = "logit"))
test_prob <- predict(logit_train,newdata = test,type = "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)
cat("Logit Training Prediction with Threshold = 0.5:"
    ,mean(test_pred == test$delinquent),"\n")
#table(True = test$delinquent,Predicted = test_pred)

thresholds <- seq(0.1, 0.9, by = 0.1)
for(th in thresholds){
  pred <- ifelse(test_prob > th, 1, 0)
  acc <- mean(pred == test$delinquent)
  cat("threshold =", th," accuracy =", acc, "\n")
}
cat("\n")
thresholds <- seq(0.48, 0.5, by = 0.001)
for(th in thresholds){
  pred <- ifelse(test_prob > th, 1, 0)
  acc <- mean(pred == test$delinquent)
  cat("threshold =", th," accuracy =", acc, "\n")
}