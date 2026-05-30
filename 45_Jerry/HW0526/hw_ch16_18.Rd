library(PoEdata)
library(sandwich)   # White robust SE
library(lmtest)     # coeftest()
data(lasvegas)

# ======================== part (a) ========================

lpm <- lm(delinquent ~ lvr + ref + insur + rate +
            amount + credit + term + arm,
          data = lasvegas)

# White heteroskedasticity-robust SE
coeftest(lpm, vcov = vcovHC(lpm, type = "HC1"))

# ======================== part (b) ========================

logit <- glm(delinquent ~ lvr + ref + insur + rate +
               amount + credit + term + arm,
             data = lasvegas,
             family = binomial(link = "logit"))

summary(logit)

# ======================== part (c) ========================

# Linear probability model predictions
pred_lpm <- predict(lpm, type = "response")
pred_lpm[c(500, 1000)]

# Logit predicted probabilities
pred_logit <- predict(logit, type = "response")
pred_logit[c(500, 1000)]

# ======================== part (d) ========================

hist(lasvegas$credit, breaks = 30,
     main = "Distribution of Credit Scores",
     xlab = "Credit score", col = "steelblue")
     
# Base scenario from part (d)
newdata <- data.frame(
  lvr    = 80,
  ref    = 0,
  insur  = 0,
  rate   = 8,
  amount = 2.5,
  credit = c(500, 600, 700),
  term   = 30,
  arm    = 0
)

# LPM predictions
pred_d_lpm <- predict(lpm, newdata = newdata)
names(pred_d_lpm) <- c("credit=500", "credit=600", "credit=700")
pred_d_lpm

# Logit predictions
pred_d_logit <- predict(logit, newdata = newdata, type = "response")
names(pred_d_logit) <- c("credit=500", "credit=600", "credit=700")
pred_d_logit

# ======================== part (e) ========================

# LPM: marginal effect is just the coefficient (constant)
lpm_credit_coef <- coef(lpm)["credit"]
cat("LPM marginal effect of CREDIT:", lpm_credit_coef, "\n")
# Same at all values of CREDIT

# Logit: marginal effect = beta * p * (1 - p)
logit_credit_coef <- coef(logit)["credit"]

p_logit <- pred_d_logit   # from part (d)
me_logit <- logit_credit_coef * p_logit * (1 - p_logit)
me_logit

# ======================== part (f) ========================

hist(lasvegas$lvr, breaks = 30,
     main = "Distribution of Loan-to-Value Ratio",
     xlab = "LVR (%)", col = "steelblue")

# Scenario: same as (d) but credit = 600, vary LVR
newdata_f <- data.frame(
  lvr    = c(20, 80),
  ref    = 0,
  insur  = 0,
  rate   = 8,
  amount = 2.5,
  credit = 600,
  term   = 30,
  arm    = 0
)

pred_f_lpm   <- predict(lpm,   newdata = newdata_f)
pred_f_logit <- predict(logit, newdata = newdata_f, type = "response")

data.frame(LVR = c(20, 80),
           LPM  = round(pred_f_lpm, 4),
           Logit = round(pred_f_logit, 4))
  
# ======================== part (g) ========================         
           
# LPM
pred_lpm_class <- ifelse(pred_lpm >= 0.5, 1, 0)
lpm_correct <- mean(pred_lpm_class == lasvegas$delinquent)
cat("LPM correct predictions:", round(lpm_correct * 100, 2), "%\n")

# Logit
pred_logit_class <- ifelse(pred_logit >= 0.5, 1, 0)
logit_correct <- mean(pred_logit_class == lasvegas$delinquent)
cat("Logit correct predictions:", round(logit_correct * 100, 2), "%\n")

# Confusion matrices
table(Predicted = pred_lpm_class,   Actual = lasvegas$delinquent)
table(Predicted = pred_logit_class, Actual = lasvegas$delinquent)

# ======================== part (h) ========================    

train <- lasvegas[1:500, ]
test  <- lasvegas[501:1000, ]

# Train logit on first 500
logit_train <- glm(delinquent ~ lvr + ref + insur + rate +
                     amount + credit + term + arm,
                   data = train,
                   family = binomial(link = "logit"))

# Predict on second 500
prob_test <- predict(logit_train, newdata = test, type = "response")

# Default threshold = 0.5
class_05 <- ifelse(prob_test >= 0.5, 1, 0)
table(Predicted = class_05, Actual = test$delinquent)

# Try different thresholds and compare
thresholds <- seq(0.1, 0.9, by = 0.05)
results <- sapply(thresholds, function(t) {
  pred <- ifelse(prob_test >= t, 1, 0)
  cm   <- table(pred, test$delinquent)
  # False negative rate = missed delinquents (bad loans approved)
  # For a lender: false negatives are costly
  fnr  <- if(ncol(cm) > 1 && nrow(cm) > 1) cm[1,2] / sum(cm[,2]) else NA
  acc  <- mean(pred == test$delinquent)
  c(threshold = t, accuracy = acc, false_neg_rate = fnr)
})

t(results)