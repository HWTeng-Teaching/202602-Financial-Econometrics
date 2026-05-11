load("mroz.rdata")
library(AER)
#(a)
cat("(a)\n")
mroz$MOTHERCOLL <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$FATHERCOLL <- ifelse(mroz$fathereduc > 12, 1, 0)

College <- (sum(mroz$MOTHERCOLL) + sum(mroz$FATHERCOLL)) / (nrow(mroz) * 2)
cat("Proportion of mothers with college:", mean(mroz$MOTHERCOLL), "\n")
cat("Proportion of fathers with college:", mean(mroz$FATHERCOLL), "\n")
cat("Percentage have college in this sample: ",College,"\n")

#(b)
cat("(b)\n")
cat("Correlation between EDUC and MOTHERCOLL: ",cor(mroz$educ, mroz$MOTHERCOLL),"\n")
cat("Correlation between EDUC and FATHERCOLL: ",cor(mroz$educ, mroz$FATHERCOLL),"\n")
cat("Correlation between MOTHERCOLL and FATHERCOLL: ",cor(mroz$MOTHERCOLL, mroz$FATHERCOLL),"\n")

#(c)
cat("(c)\n")
alpha <- 0.05
iv_model <- ivreg(wage ~ educ | MOTHERCOLL, data = mroz)
b2 <- coef(iv_model)[[2]]
se <- sqrt(vcov(iv_model)[2,2])

df <- df.residual(iv_model)
tcTwoTail <- qt(1-alpha/2, df)
lower <- b2 - tcTwoTail * se
upper <- b2 + tcTwoTail * se
cat("Confidence Interval:[",lower,",",upper,"]\n")

#(d)
cat("(d)\n")
first_stage <- lm(educ ~ MOTHERCOLL, data = mroz)
b2first_modD <- coef(first_stage)[[2]]
b1first_modD <- coef(first_stage)[[1]]
cat("educ = ",b1first_modD,"+",b2first_modD,"* MOTHERCOLL\n")
F_stat <- summary(first_stage)$fstatistic[1]
cat("F statistic:", F_stat, "\n")

#(e)
cat("(e)\n")
iv_model2 <- ivreg(wage ~ educ | MOTHERCOLL + FATHERCOLL, data = mroz)
b2mod2 <- coef(iv_model2)[[2]]
semod2 <- sqrt(vcov(iv_model2)[2,2])

df <- df.residual(iv_model2)
tcTwoTailmod2 <- qt(1-alpha/2, df)
lowermod2 <- b2mod2 - tcTwoTailmod2 * semod2
uppermod2 <- b2mod2 + tcTwoTailmod2 * semod2
cat("Confidence Interval:[",lowermod2,",",uppermod2,"]\n")

#(f)
cat("(f)\n")
first_modF <- lm(educ~MOTHERCOLL + FATHERCOLL,data = mroz)
b3first_modF <- coef(first_modF)[[3]]
b2first_modF <- coef(first_modF)[[2]]
b1first_modF <- coef(first_modF)[[1]]
cat("educ = ",b1first_modF,"+",b2first_modF,"* MOTHERCOLL+",b3first_modF,"* FATHERCOLL\n")
F_statF <- summary(first_modF)$fstatistic[1]
cat("F statistic:", F_statF, "\n")

#(g)
cat("(g)\n")
diag <- summary(iv_model2, diagnostics = TRUE)$diagnostics
cat("P-value of Sargen test:",diag["Sargan", "p-value"],"\n")