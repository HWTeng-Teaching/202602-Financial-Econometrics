remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("mroz",package="PoEdata")
install.packages("AER")
library(AER)
library(car)
summary(mroz)

#10.18.a
mroz_sub$MOTHERCOLL <- ifelse(mroz_sub$mothereduc > 12, 1, 0)
mroz_sub$FATHERCOLL <- ifelse(mroz_sub$fathereduc > 12, 1, 0)
mean(mroz_sub$MOTHERCOLL, na.rm = TRUE)  
mean(mroz_sub$FATHERCOLL, na.rm = TRUE)

#10.18.b
cor(mroz_sub[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")

#10.18.c
mroz_sub$lwage <- log(mroz_sub$wage)
mroz_sub$exper2 <- mroz_sub$exper^2
iv_model <- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + MOTHERCOLL, data = mroz_sub)
summary(iv_model,diagnostics = TRUE)
confint(iv_model, level = 0.95)["educ", ]
cat('95% Confidence Interval: [',confint(iv_model, level = 0.95)["educ", ],']')

#10.18.d
first_stage <- lm(educ ~ exper + exper2 + MOTHERCOLL, data = mroz_sub)
summary(first_stage)
Anova(first_stage, type = "II")

#10.18.e
iv_model_2<- ivreg(lwage ~ exper + exper2 + educ |
                          exper + exper2 + MOTHERCOLL + FATHERCOLL,
                        data = mroz_sub)

summary(iv_model_2, diagnostics = TRUE)
confint(iv_model_2, level = 0.95)["educ", ]

#10.18.f
first_stage_2 <- lm(educ ~ exper + exper2 + MOTHERCOLL + FATHERCOLL, data = mroz_sub)
summary(first_stage_2)
Anova(first_stage_2, type = "II")
linearHypothesis(first_stage_2, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#10.18.g
resid_iv <- resid(iv_model_2)
sargan_test <- lm(resid_iv ~ MOTHERCOLL + FATHERCOLL, data = mroz_sub)
S <- nrow(mroz_sub) * summary(sargan_test)$r.squared
(p_value <- 1 - pchisq(S, df = 1))

#10.20.a
summary(capm5)
capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(capm_model)

#10.20.b
capm5$RANK <- rank(capm5$mkt_excess)
first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)
Anova(first_stage, type = "II")

#10.20.c
capm5$v_hat <- resid(first_stage)
capm_with_residuals <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(capm_with_residuals)

#10.20.d
iv_model <- ivreg(msft_excess ~ mkt_excess | RANK, data = capm5)
summary(iv_model)

#10.20.e
capm5$pos <- ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage_pos <- lm(mkt_excess ~ RANK + pos, data = capm5)
summary(first_stage_pos)
anova(first_stage_pos)
linearHypothesis(first_stage_pos, c("RANK = 0", "pos = 0"))

#10.20.g
iv_model2 <- ivreg(msft_excess ~ mkt_excess | rank + pos, data = capm5)
summary(iv_model2)

#10.20.f
capm5$v_hat2 <- resid(first_stage_pos)
capm_with_residuals2 <- lm(msft_excess ~ mkt_excess + v_hat2, data = capm5)
summary(capm_with_residuals2)

#10.20.g
iv_model2 <- ivreg(msft_excess ~ mkt_excess | rank + pos, data = capm5)
summary(iv_model2)

#10.20.h
capm5$resid_iv2 <- resid(iv_model2)
sargan_reg <- lm(resid_iv2 ~ rank+ pos, data = capm5)
summary(sargan_reg)
R2 <- summary(sargan_reg)$r.squared
n <- nrow(capm5)
S <- n * R2
n
S
(p_value <- 1 - pchisq(S, df = 1))
cat('R squared=',R2,'\nnRsquared=',S,'\np-value=',p_value,'\nConclusion:\nAt the 5% level, we do not reject the null hypothesis that the instruments are valid. Both RANK and POS are considered exogenous.' )

