load("capm5.rdata")
library(AER)
#(a)
cat("(a)\n")
ExcessR = capm5$msft - capm5$riskfree
ExcessM = capm5$mkt - capm5$riskfree
mod1 <- lm(ExcessR ~ ExcessM)
b2 <- coef(mod1)[[2]]
b1 <- coef(mod1)[[1]]
cat("R_i - R_f = ",b1,"+",b2,"* (R_m - R_f)\n")

#(b)
cat("(b)\n")
capm5$ExcessM = capm5$mkt - capm5$riskfree
capm5$RANK <- rank(capm5$ExcessM)
first_stage <- lm(ExcessM ~ RANK, data=capm5)
print(summary(first_stage))
R2_b = summary(first_stage)$r.squared
b2B <- coef(first_stage)[[2]]
b1B <- coef(first_stage)[[1]]
cat("R_i - R_f = ",b1B,"+",b2B,"* (R_m - R_f)\n")
cat("R-squared =", R2_b, "\n")


#(c)
cat("(c)\n")
vhat <- resid(first_stage)
modC <- lm(ExcessR ~ ExcessM + vhat, data=capm5)
print(summary(modC))
b3C <- coef(modC)[[3]]
b2C <- coef(modC)[[2]]
b1C <- coef(modC)[[1]]
cat("R_i - R_f = ",b1C,"+",b2C,"* (R_m - R_f)",b3C,"* vHat\n")
cat("P-value for residual:",summary(modC)$coefficients[[3,4]],"\n")

#(d)
cat("(d)\n")
iv_model <- ivreg(ExcessR ~ ExcessM | RANK, data = capm5)
b2D <- coef(iv_model)[[2]]
b1D <- coef(iv_model)[[1]]
cat("R_i - R_f = ",b1D,"+",b2D,"* (R_m - R_f)\n")

#(e)
cat("(e)\n")
capm5$POS <- ifelse(capm5$ExcessM > 0, 1, 0)
first_stageE <- lm(ExcessM ~ RANK + POS, data=capm5)
b3E <- coef(first_stageE)[[3]]
b2E <- coef(first_stageE)[[2]]
b1E <- coef(first_stageE)[[1]]
cat("R_m - R_f = ",b1E,"+",b2E,"* RANK + ",b3E,"* POS\n")
print(summary(first_stageE))
R2E = summary(first_stageE)$r.squared
cat("R-squared =", R2E, "\n")

#(f)
cat("(f)\n")
vhatE <- resid(first_stageE)
modF <- lm(ExcessR ~ ExcessM + vhatE, data=capm5)
print(summary(modF))
cat("p-value for Hausman test:",summary(modF)$coefficients[[3,4]],"\n")

#(g)
cat("(g)\n")
iv_modelG <- ivreg(ExcessR ~ ExcessM | RANK + POS, data = capm5)
b2G <- coef(iv_modelG)[[2]]
b1G <- coef(iv_modelG)[[1]]
cat("R_i - R_f = ",b1G,"+",b2G,"* (R_m - R_f)\n")

#(h)
cat("(h)\n")
vhatH <- resid(iv_modelG)
sargan_reg <- lm(vhatH ~ RANK + POS, data = capm5)
N <- nrow(capm5)
R2 <- summary(sargan_reg)$r.squared
J <- N * R2
p_value <- 1 - pchisq(J, 2-1)
print(summary(iv_modelG, diagnostics = TRUE))
cat("Sargan statistic =", J, "\n")
cat("p-value =", p_value, "\n")
