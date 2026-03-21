rm(list=ls())
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
install.packages("stargazer")
library(stargazer)
data('star5_small')


# 2.22
#a
lin_model = lm(totalscore~small,data=star5_small)
summary(lin_model)
stargazer(lin_model,
          type = "html",
          out = "STAR_Regression_Table.doc",
          title = "Table 1: Effect of Class Size on Student Aptitude",
          dep.var.labels = "Total Score",
          covariate.labels = c("Small Class"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))
# b.
lin_model2 = lm(readscore~small,data=star5_small)
summary(lin_model2)
stargazer(lin_model2,
          type = "html",
          out = "Table2.doc",
          title = "Table 2: Effect of Class Size on Student Aptitude",
          dep.var.labels = "Read Score",
          covariate.labels = c("Small Class"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

lin_model3 = lm(mathscore~small,data=star5_small)
summary(lin_model3)
stargazer(lin_model3,
          type = "html",
          out = "Table3.doc",
          title = "Table 3: Effect of Class Size on Student Aptitude",
          dep.var.labels = "Math Score",
          covariate.labels = c("Small Class"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))
# c.
lin_model4 = lm(totalscore~aide,data=star5_small)
summary(lin_model4)
stargazer(lin_model4,
          type = "html",
          out = "Table4.doc",
          title = "Table 4: Effect of AIDE on Student Aptitude",
          dep.var.labels = "Total Score",
          covariate.labels = c("AIDE"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))
# d.
lin_model5 = lm(readscore~aide,data=star5_small)
summary(lin_model5)
stargazer(lin_model5,
          type = "html",
          out = "Table5.doc",
          title = "Table 5: Effect of AIDE on Student Aptitude",
          dep.var.labels = "Read Score",
          covariate.labels = c("AIDE"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

lin_model6 = lm(mathscore~aide,data=star5_small)
summary(lin_model6)
stargazer(lin_model6,
          type = "html",
          out = "Table6.doc",
          title = "Table 6: Effect of AIDE on Student Aptitude",
          dep.var.labels = "Math Score",
          covariate.labels = c("AIDE"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

#2.25
data("cex5_small")

str(cex5_small)
summary(cex5_small$foodaway)
hist(cex5_small$foodaway,breaks=30,main = 'Histogram of foodaway',xlab = 'FOODAWAY(Dollar per month per person)')

cex5_small$advanced = as.logical(cex5_small$advanced)
advanced_foodaway = cex5_small$foodaway[cex5_small$advanced]
cat("Summary of advanced_foodaway:\n")
print(summary(advanced_foodaway))

cat("N (Advanced):", length(advanced_foodaway), "\n\n")
cex5_small$college = as.logical(cex5_small$college)
college_foodaway = cex5_small$foodaway[cex5_small$college]
cat("Summary of college_foodaway:\n")
print(summary(college_foodaway))

cat("N (College):", length(college_foodaway), "\n\n")
non_foodaway = cex5_small$foodaway[!cex5_small$college & !cex5_small$advanced]
cat("Summary of non_foodaway:\n")
print(summary(non_foodaway))
cat("N (Non-college and Non-advanced):", length(non_foodaway), "\n")



cex5_small$lnfoodaway = ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)
length(cex5_small$foodaway)
sum(!is.na(cex5_small$lnfoodaway))
summary(cex5_small$lnfoodaway)
hist_ln = hist(cex5_small$lnfoodaway,
               main = "Histogram of ln(FOODAWAY)",
               xlab = "ln(FOODAWAY)",
               ylab = "Frequency",
               col = "blue",
               border = "black",
               freq = TRUE,
               breaks = 30)



linear_model_foodaway = lm(lnfoodaway ~ income, data = cex5_small)
summary(linear_model_foodaway)
stargazer(linear_model_foodaway,
          type = "html",
          out = "Table7.doc",
          title = "Table 7",
          dep.var.labels = "ln(foodaway)",
          covariate.labels = c("INCOME"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

plot(cex5_small$income,cex5_small$lnfoodaway,pch=16,cex=0.5,xlab = 'Income',ylab = 'ln(foodaway)')
abline(linear_model_foodaway, col = "red",lwd=2)

resid_values = summary(linear_model_foodaway)$resid
valid_idx = !is.na(resid_values) & !is.na(cex5_small$income)
resid_square = sum(resid_values[valid_idx]^2)
plot(
  cex5_small$income[valid_idx],
  resid_values[valid_idx],
  pch = 16,
  cex = 0.5,
  xlab = "Income",
  ylab = "Residuals"
)
