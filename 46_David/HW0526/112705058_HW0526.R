library(sandwich)
library(lmtest)

# =====================================================
# 16.18  Las Vegas mortgage delinquency (N = 1000)
#   DELINQUENT = 1 if >= 3 missed payments (90+ days late)
#   Regressors: LVR REF INSUR RATE AMOUNT CREDIT TERM ARM
# =====================================================
url <- "https://github.com/ccolonescu/PoEdata/raw/master/data/lasvegas.rda"
tmp <- tempfile(fileext = ".rda")
download.file(url, tmp, mode = "wb")
load(tmp)                       # loads object 'lasvegas'
lv  <- lasvegas
names(lv) <- tolower(names(lv))
cat("\n[16.18] dim =", dim(lv), "\n"); print(names(lv))

f <- delinquent ~ lvr + ref + insur + rate + amount + credit + term + arm

#a  Linear probability model with White robust SE
lpm <- lm(f, data = lv)
lpm_rob <- coeftest(lpm, vcov. = vcovHC(lpm, type = "HC1"))
cat("\n(a) LPM with White (HC1) robust SE:\n"); print(lpm_rob)
# Expected signs: LVR(+), REF(+/?), INSUR(-), RATE(+), AMOUNT(+/?),
#                 CREDIT(-), TERM(?), ARM(+)

#b  Logit
logit <- glm(f, data = lv, family = binomial(link = "logit"))
cat("\n(b) Logit estimates:\n"); print(coeftest(logit))
cat("\nSign / significance comparison (LPM vs logit):\n")
cmp <- data.frame(
  lpm_coef  = coef(lpm),
  lpm_sig   = lpm_rob[, 4] < 0.05,
  logit_coef = coef(logit),
  logit_sig  = summary(logit)$coef[, 4] < 0.05)
print(round(cmp[, c(1,3)], 5)); print(cmp[, c(2,4)])

#c  Predicted DELINQUENT for obs 500 and 1000, both models
obs <- c(500, 1000)
pred_c <- data.frame(
  obs   = obs,
  lpm   = predict(lpm,   newdata = lv[obs, ]),
  logit = predict(logit, newdata = lv[obs, ], type = "response"))
cat("\n(c) Predicted probabilities for obs 500 & 1000:\n")
print(round(pred_c, 4))

#d  Probability of delinquency for CREDIT = 500/600/700
#   AMOUNT=2.5, LVR=80, RATE=8, TERM=30, all indicators (REF,INSUR,ARM)=0
new_d <- data.frame(lvr = 80, ref = 0, insur = 0, rate = 8,
                    amount = 2.5, credit = c(500, 600, 700),
                    term = 30, arm = 0)
pred_d <- data.frame(
  credit = new_d$credit,
  lpm    = predict(lpm,   newdata = new_d),
  logit  = predict(logit, newdata = new_d, type = "response"))
cat("\n(d) P(delinquent) by CREDIT (other vars fixed):\n")
print(round(pred_d, 4))
# histogram of CREDIT
hist(lv$credit, main = "Histogram of CREDIT", xlab = "CREDIT", col = "grey80")

#e  Marginal effect of CREDIT at CREDIT = 500/600/700 (other vars as in d)
b_credit <- coef(lpm)["credit"]
g_credit <- coef(logit)["credit"]
xb <- predict(logit, newdata = new_d)             # linear index
lam <- plogis(xb)
me_e <- data.frame(
  credit    = new_d$credit,
  lpm_me    = rep(b_credit, 3),                   # constant
  logit_me  = g_credit * lam * (1 - lam))         # varies
cat("\n(e) Marginal effect of CREDIT (per 1-unit credit score):\n")
print(round(me_e, 6))

#f  Probability of delinquency for LVR = 20 vs 80 (CREDIT = 600, others as d)
new_f <- data.frame(lvr = c(20, 80), ref = 0, insur = 0, rate = 8,
                    amount = 2.5, credit = 600, term = 30, arm = 0)
pred_f <- data.frame(
  lvr   = new_f$lvr,
  lpm   = predict(lpm,   newdata = new_f),
  logit = predict(logit, newdata = new_f, type = "response"))
cat("\n(f) P(delinquent) for LVR = 20 vs 80:\n")
print(round(pred_f, 4))
hist(lv$lvr, main = "Histogram of LVR", xlab = "LVR", col = "grey80")

#g  Percentage of correct predictions, threshold 0.5
yhat_lpm   <- as.integer(fitted(lpm)   > 0.5)
yhat_logit <- as.integer(fitted(logit) > 0.5)
acc_lpm   <- mean(yhat_lpm   == lv$delinquent)
acc_logit <- mean(yhat_logit == lv$delinquent)
cat("\n(g) Percentage correctly predicted (threshold 0.5):\n")
cat(sprintf("    LPM   = %.4f\n", acc_lpm))
cat(sprintf("    logit = %.4f\n", acc_logit))
cat("\nConfusion tables:\n")
cat("LPM:\n");   print(table(pred = yhat_lpm,   actual = lv$delinquent))
cat("logit:\n"); print(table(pred = yhat_logit, actual = lv$delinquent))

#h  Train on obs 1-500, evaluate decision rule on obs 501-1000
train <- lv[1:500, ]
test  <- lv[501:1000, ]
logit_tr <- glm(f, data = train, family = binomial(link = "logit"))
p_test   <- predict(logit_tr, newdata = test, type = "response")

# default rule: approve (predict non-delinquent) if p < 0.5
rule_05 <- as.integer(p_test > 0.5)
cat("\n(h) Out-of-sample (501-1000), threshold 0.5:\n")
print(table(pred = rule_05, actual = test$delinquent))
cat("base rate of delinquency in test set:",
    round(mean(test$delinquent), 4), "\n")

# alternative: choose threshold minimizing total error / lender loss
thr <- seq(0.05, 0.95, by = 0.05)
err <- sapply(thr, function(t)
  mean(as.integer(p_test > t) != test$delinquent))
cat("\nError rate by threshold:\n")
print(round(data.frame(threshold = thr, error = err), 4))
cat("threshold minimizing error:", thr[which.min(err)], "\n")
