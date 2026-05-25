load("Documents/R/data_needed/poe5rdata/star.rdata")
#------------------------------------------------------------------------
# part a

# Estimate a regression equation (with no fixed or random effect)
star_pooled <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

# Print the result
summary(star_pooled)
#------------------------------------------------------------------------
# part b

# 把資料轉成 plm 看得懂的 panel data 格式
# SCHID as cross-section identifier, ID as time identifier
star_panel <- pdata.frame(star, index = c("schid", "id"))

# Estimate the model with fixed effect
star_fixed <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                   data = star_panel,
                   model = "within")

# Print the result
summary(star_fixed)
#------------------------------------------------------------------------
# part c

pFtest(star_fixed, star_pooled)
#------------------------------------------------------------------------
# part d

# Estimate the model with random effect
star_random <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                   data = star_panel,
                   model = "random")

# Print the result
summary(star_random)

# Execute LM test
star_pooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                   data = star_panel)
startest <- plmtest(star_pooled, effect = "individual")
print(startest)
#------------------------------------------------------------------------
# part e

# Extract data form FE and RE models
summary_fe <- summary(star_fixed)
summary_re <- summary(star_random)

# Define the variables
variables_to_test <- c("small", "aide", "tchexper", "white_asian", "boy", "freelunch")

# Extract the coefficient and std.error of these variables
b_fe <- summary_fe$coefficients[variables_to_test, "Estimate"]
se_fe <- summary_fe$coefficients[variables_to_test, "Std. Error"]

b_re <- summary_re$coefficients[variables_to_test, "Estimate"]
se_re <- summary_re$coefficients[variables_to_test, "Std. Error"]

# t test
t_stats <- (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)

print(t_stats)
#------------------------------------------------------------------------
# part f

# Create a new variable : school-averages
star_mundlak <- star %>%
  group_by(schid) %>%
  mutate(
    mean_small = mean(small, na.rm = TRUE),
    mean_aide = mean(aide, na.rm = TRUE),
    mean_tchexper = mean(tchexper, na.rm = TRUE),
    mean_boy = mean(boy, na.rm = TRUE),
    mean_white_asian = mean(white_asian, na.rm = TRUE),
    mean_freelunch = mean(freelunch, na.rm = TRUE)
  ) %>%
  ungroup()

# Transform the new data set into panel data
star_panel_mundlak <- pdata.frame(star_mundlak, index = c("schid", "id"))

# 跑 Mundlak 的隨機效果模型
star_random_mundlak <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                             mean_small + mean_aide + mean_tchexper + mean_boy + mean_white_asian + mean_freelunch,
                           data = star_panel_mundlak,
                           model = "random",
                           random.method = "walhus")

# Execute Mundlak test
linearHypothesis(star_random_mundlak, 
                 c("mean_small = 0", 
                   "mean_aide = 0", 
                   "mean_tchexper = 0", 
                   "mean_boy = 0", 
                   "mean_white_asian = 0", 
                   "mean_freelunch = 0"))




