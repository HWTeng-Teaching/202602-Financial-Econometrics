library(POE5Rdata)
library(AER)
data("mroz")

#a
lfp <- mroz$lfp
mother_educ <- mroz$mothereduc
father_educ <- mroz$fathereduc

mothercoll <- ifelse(mother_educ > 12 & lfp == 1, 1, 0)
fathercoll <- ifelse(father_educ > 12 & lfp == 1, 1, 0)

numbers_mothercoll <- sum(mothercoll)
numbers_fathercoll <- sum(fathercoll)

percentage_mothercoll <- (numbers_mothercoll / 428) * 100
percentage_fathercoll <- (numbers_fathercoll / 428) * 100

print(paste("Percentage of mothers with some college education:", percentage_mothercoll, "%"))
print(paste("Percentage of fathers with some college education:", percentage_fathercoll, "%"))

#b
mothercoll <- mothercoll[1 : 428]
fathercoll <- fathercoll[1 : 428]
educ <- mroz$educ
educ <- educ[1 : 428]
correlation_matrix <- cor(cbind(educ,mothercoll, fathercoll))
print(correlation_matrix)

#c
#限制在married women
mroz1 <- mroz[mroz$lfp==1,] 
mroz1$MOTHERCOLL <- ifelse(mroz1$mothereduc > 12, 1, 0)

wage_iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll + exper + I(exper^2), data = mroz1)
educ_95percent_interval <- confint(wage_iv_model, level = 0.95)["educ",]
confint(wage_iv_model, "educ", level = 0.95)

#d
wage_ols_model <- lm(educ ~ mothercoll, data = mroz1)
f_test <- summary(wage_ols_model)$fstatistic[1]
cat("F-test statistic for the hypothesis that MOTHERCOLL has no effect on EDUC:", f_test, "\n")

#e
# 篩選 working women
mroz_sub <- subset(mroz, wage > 0)
#檢查樣本數
nrow(mroz_sub)

iv_model <- ivreg(log(wage) ~ exper+I(exper^2)+educ | exper+I(exper^2)+mothercoll+fathercoll, data=mroz_sub)
summary(iv_model)
confint(iv_model, "educ", level=0.95)

#f
# 篩選 working women
mroz <- subset(mroz, wage > 0)
# 確認長度一致
sapply(mroz, length)

mroz.lm2 <- lm(educ ~ mothercoll + fathercoll, data = mroz)
f_test_f <- summary(mroz.lm2)$fstatistic[1]

cat("F-test statistic for joint significance of MOTHERCOLL and FATHERCOLL:", f_test_f, "\n")

#g
diag_mat <- summary(iv_model, diagnostics = TRUE)$diagnostics
g_test <- diag_mat[grep("Sargan", rownames(diag_mat)), "statistic"]
critical_value <- qchisq(0.95, df=1) 
print(critical_value)
cat("Sargan-Hansen statistic for the validity of the surplus instrument:", g_test, "\n")