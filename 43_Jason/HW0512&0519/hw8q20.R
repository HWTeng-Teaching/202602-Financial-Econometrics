# Chan Nok Hang 414707007
#hw8 ch15-Q20
# Load the data
library(POE5Rdata)
data(star)
library(plm)
library(car)

# Part A
# standard pooled OLS regression
ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(ols_model)

# Part B
# Specify the panel data structure
names(star)[names(star) == "id"] <- "student_id"
pdata_star <- pdata.frame(star, index = c("schid", "student_id"))

# Estimate the Fixed Effects (within) model
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = pdata_star, model = "within")
summary(fe_model)

# Part C
pFtest(fe_model, ols_model)

# Part D
# Estimate the Random Effects model
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = pdata_star, model = "random")
summary(re_model)

# Breusch-Pagan LM test for random effects, if not worked use the below
#plmtest(ols_model, type = "bp", effect = "individual")

# Corrected Breusch-Pagan LM test for random effects
plmtest(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
        data = pdata_star, effect = "individual", type = "bp")

# Part F
# Step 1: Calculate school-level means using the built-in 'ave' function
star$mean_small       <- ave(star$small, star$schid, FUN = function(x) mean(x, na.rm = TRUE))
star$mean_aide        <- ave(star$aide, star$schid, FUN = function(x) mean(x, na.rm = TRUE))
star$mean_tchexper    <- ave(star$tchexper, star$schid, FUN = function(x) mean(x, na.rm = TRUE))
star$mean_boy         <- ave(star$boy, star$schid, FUN = function(x) mean(x, na.rm = TRUE))
star$mean_white_asian <- ave(star$white_asian, star$schid, FUN = function(x) mean(x, na.rm = TRUE))
star$mean_freelunch   <- ave(star$freelunch, star$schid, FUN = function(x) mean(x, na.rm = TRUE))

# Step 2&3: Run the Mundlak Model

# Define the new panel data structure using the updated star dataset
pdata_mundlak <- pdata.frame(star, index = c("schid", "student_id"))

# Run the Mundlak auxiliary regression using standard OLS
mundlak_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch + 
                    mean_small + mean_aide + mean_tchexper + mean_boy + mean_white_asian + mean_freelunch, 
                  data = star)

# (Optional) View the regression to see the individual coefficients
summary(mundlak_ols)

# Step 4: Perform the joint test on the school averages
# We test if the coefficients on all 'mean_' variables are jointly zero
linearHypothesis(mundlak_ols, c("mean_small = 0", 
                                  "mean_aide = 0", 
                                  "mean_tchexper = 0", 
                                  "mean_boy = 0", 
                                  "mean_white_asian = 0", 
                                  "mean_freelunch = 0"))

# Perform the joint test but force it to output a Chi-Square statistic
linearHypothesis(mundlak_ols, c("mean_small = 0", 
                                "mean_aide = 0", 
                                "mean_tchexper = 0", 
                                "mean_boy = 0", 
                                "mean_white_asian = 0", 
                                "mean_freelunch = 0"), 
                 test = "Chisq")