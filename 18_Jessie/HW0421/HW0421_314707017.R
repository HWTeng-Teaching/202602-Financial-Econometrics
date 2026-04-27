#10.18

library(POE5Rdata)
data("mroz")

install.packages(c("wooldridge","AER","car","lmtest","sandwich","boot"))
library(AER)
library(car)
library(lmtest)
library(sandwich)

#(a)
married_data = mroz[mroz$lfp==1, ]
married_data$mothercoll <- ifelse(married_data$mothereduc > 12, 1, 0)
married_data$fathercoll <- ifelse(married_data$fathereduc > 12, 1, 0)
pct_mother <- mean(married_data$mothercoll) * 100
pct_father <- mean(married_data$fathercoll) * 100

cat("母親有大學教育者比例：", round(pct_mother, 2), "%\n")
cat("父親有大學教育者比例：", round(pct_father, 2), "%\n")

#(b)
corr_matrix <- cor(married_data[, c("educ", "mothercoll", "fathercoll")])
print(round(corr_matrix, 4))

#(c)
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll + exper + I(exper^2), data = married_data)
summary(iv_model,level=0.95)
confint(iv_model, level = 0.95)

#(d)
summary(iv_model, diagnostics = TRUE)

#(e)
iv2_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll+fathercoll + exper + I(exper^2), data = married_data)
summary(iv2_model)
confint(iv2_model, level = 0.95)

#(f,g)
summary(iv2_model, diagnostics = TRUE)




#10.20
library(POE5Rdata)
data("capm5")

#(a)
capm5$msft_excess=capm5$msft - capm5$riskfree
capm5$mkt_excess=capm5$mkt  - capm5$riskfree

model=lm(msft_excess ~ mkt_excess, data = capm5)

summary(model)

#b
capm5$RANK=rank(capm5$mkt_excess, ties.method = "first")
first_stage=lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)

#c
capm5$vhat=resid(first_stage)
model2=lm(msft_excess ~ mkt_excess + vhat, data = capm5)
summary(model2)


#(d)
library(AER)

iv_model=ivreg(msft_excess ~ mkt_excess |
                 RANK,
               data = capm5)

summary(iv_model)


#(e)
capm5$msft_excess=capm5$msft - capm5$riskfree
capm5$mkt_excess=capm5$mkt  - capm5$riskfree
capm5$RANK=rank(capm5$mkt_excess, ties.method = "first")

#e
capm5$POS=ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage=lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage)

#f
capm5$vhat2=resid(first_stage)

hausman_model= lm(msft_excess ~ mkt_excess + vhat2, data = capm5)
summary(hausman_model)

#g
library(AER)

iv_final=ivreg(msft_excess ~ mkt_excess |
                 RANK + POS,
               data = capm5)

summary(iv_final)

#h
iv_model=ivreg(msft_excess ~ mkt_excess |
                 RANK,
               data = capm5)
uhat=resid(iv_model)

sargan_test=lm(uhat ~ RANK + POS, data = capm5)
summary(sargan_test)

n=nrow(capm5)
R2= summary(sargan_test)$r.squared
S= n * R2
S

qchisq(0.95, df = 1)