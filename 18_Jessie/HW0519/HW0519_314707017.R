


#15.20
```{r echo=TRUE, message=FALSE, warning=FALSE}
rm(list=ls())
library(POE5Rdata)
library(plm)
library(nlme)

# a
data("star")
star.pd <- pdata.frame(star, index = c("schid"))
# Estimate a pooled OLS model
model.pooling <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,data = star.pd, model = "pooling")
summary(model.pooling)

# b
star.pd <- pdata.frame(star, index = c("schid")) 
model.within <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star.pd, model = "within")
summary(model.within)

# c
pFtest(model.within, model.pooling)
df1 = 78
df2 = 5681
qf(0.95, df1, df2)


#e
setwd("C:\\Users\\58423\\OneDrive\\¤å¥ó\\R HW")
data = read.csv("star.csv")
install.packages("plm")
install.packages("lmtest")
library(plm)
library(lmtest)
panel_data = pdata.frame(data, index = c("schid", "id"))
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = panel_data, model = "within")
summary(fe_model)
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = panel_data, model = "random")
summary(re_model)
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)
# Extract coefficients and standard errors
fe_coef <- summary(fe_model)$coefficients
re_coef <- summary(re_model)$coefficients

# Function to calculate t-statistic for each coefficient
t_test_coef <- function(fe_coef, re_coef) {
  t_values <- (fe_coef[, "Estimate"] - re_coef[, "Estimate"]) / 
    sqrt(fe_coef[, "Std. Error"]^2 + re_coef[, "Std. Error"]^2)
  p_values <- 2 * pt(-abs(t_values), df = nrow(panel_data) - length(fe_coef))
  return(data.frame(t_values, p_values))
}

# Apply the t-test function
coef_test_results <- t_test_coef(fe_coef, re_coef)
print(coef_test_results)

# Extract the coefficient and standard error for BOY
fe_boy <- fe_coef["boy", ]
re_boy <- re_coef["boy", ]

# Calculate t-statistic and p-value for BOY
t_value_boy <- (fe_boy["Estimate"] - re_boy["Estimate"]) / 
  sqrt(fe_boy["Std. Error"]^2 + re_boy["Std. Error"]^2)
p_value_boy <- 2 * pt(-abs(t_value_boy), df = nrow(panel_data) - 1)

# Print results for BOY
cat("T-statistic for BOY:", t_value_boy, "\n")
cat("P-value for BOY:", p_value_boy, "\n")


#f
load("/Users/ivywu/Downloads/poe5rdata/star.rdata")

star$stuid <- star$id
install.packages("plm")
library(plm)
install.packages("car")
library(car)

# Get average of variables by teacher
teacher.mean <- aggregate(cbind(boy, white_asian, freelunch) ~ tchid, star, mean) # Rename columns to indicate they are averages
names(teacher.mean) <- paste(names(teacher.mean), c("", "m", "m", "m"), sep = "") # Merge averages with original panel data
star.m <- merge(star, teacher.mean, on = "tchid")
# Estimate random effects with averaged variables
plm.random.m <- plm(readscore ~ small + aide + tchexper + tchmasters + boy +
                      white_asian + freelunch + boym + white_asianm + freelunchm,
                    data = star.m,
                    index = c("tchid", "stuid"),
                    model = "random",
                    random.method = "walhus")
# Test that average variables are jointly different from 0
linearHypothesis(plm.random.m, c("boym = 0", "white_asianm = 0", "freelunchm = 0"))