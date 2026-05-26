#15.20
#a
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"))
mod_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star)
summary(mod_a)

#b

# Rename 'id' to 'stuid' to avoid conflict with plm's reserved index name 'id'
star2 <- star
star2$stuid <- star2$id
star2$id    <- NULL

install.packages("plm")
library(plm)
star_panel <- pdata.frame(star2, index = c("schid", "stuid"))
fe <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
         data = star_panel,
         model="within",
         effect = "individual")
summary(fe)
#c
pFtest(fe,mod_a)
re <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data = star_panel,
  model = "random",
  effect = "individual")

summary(re)

# Compare coefficients across all three models
coef_all <- cbind(
  OLS = coef(mod_a)[-1],
  FE  = coef(fe),
  RE  = coef(re)[-1]   # Drop intercept
)
# LM test: H0 = no random effects (Breusch-Pagan LM test)
plmtest(re, type = "bp")
 #e Hausman test

HM_test <-phtest(fe, re)

vars <- c("small", "aide", "tchexper", "white_asian", "freelunch")
b_fe  <- coef(fe)[vars]
b_re  <- coef(re)[vars]
se_fe <- sqrt(diag(vcov(fe)))[vars]
se_re <- sqrt(diag(vcov(re)))[vars]
t_stat <- (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)
cat("\n--- t-statistics (FE - RE), 5% significance level, critical value ≈ ±1.96 ---\n")
result_e <- data.frame(
  b_FE      = round(b_fe, 4),
  b_RE      = round(b_re, 4),
  t_stat    = round(t_stat, 4),
  reject_H0 = abs(t_stat) > 1.96
)
print(result_e)

# Apply the test separately to BOY
b_fe_boy  <- coef(fe)["boy"]
b_re_boy  <- coef(re)["boy"]
se_fe_boy <- sqrt(vcov(fe)["boy", "boy"])
se_re_boy <- sqrt(vcov(re)["boy", "boy"])

t_boy <- (b_fe_boy - b_re_boy) / sqrt(se_fe_boy^2 - se_re_boy^2)

#f Mundlak Test

mundlak_vars <- c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")

school_means <- aggregate(star2[, mundlak_vars],
                          by  = list(schid = star2$schid),
                          FUN = mean)
colnames(school_means)[-1] <- paste0(mundlak_vars, "_mean")

star_mundlak <- merge(star2, school_means, by = "schid")

mundlak_model <- lm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
    small_mean + aide_mean + tchexper_mean +
    boy_mean + white_asian_mean + freelunch_mean,
  data = star_mundlak
)

summary(mundlak_model)

# H0: all mean coefficients = 0 (no correlation with unobserved heterogeneity)
mean_vars <- c("small_mean", "aide_mean", "tchexper_mean",
               "boy_mean", "white_asian_mean", "freelunch_mean")
library(car)
linearHypothesis(mundlak_model,
                 hypothesis.matrix = mean_vars,
                 test = "Chisq")
mean_coefs <- summary(mundlak_model)$coefficients[mean_vars, ]
