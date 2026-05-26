library(plm)
library(lmtest)
library(car)
library(sandwich)
library(PoEdata)
data("star")

# (a) Pooled OLS
eq_base <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch
ols <- lm(eq_base,  data = star)
summary(ols)

# (b) School Fixed Effects
pdata <- pdata.frame(star, index = c("schid"))

fe <- plm(eq_base, data = pdata, model = "within")
summary(fe)

# (c) F test for school FE
anova(ols, fe_lm)

# (d) Random Effects and LM test
re_mod <- plm(eq_base,  data = pdata, model = "random")
summary(re_mod)

pooled_plm <- plm(eq_base, data = pdata, model = "pooling")

plmtest(pooled_plm, effect = "individual", type = "bp")

# (e) Joint Hausman test: supplementary
phtest(fe, re_mod)

#Single-coefficient Hausman t-tests 
coef_fe <- coef(fe)
coef_re <- coef(re_mod)

se_fe <- sqrt(diag(vcov(fe)))
se_re <- sqrt(diag(vcov(re_mod)))

vars <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

t_test <- (coef_fe[vars] - coef_re[vars]) /  sqrt(abs(se_fe[vars]^2 - se_re[vars]^2))

t_test

# (f) Mundlak test
star$m_small <- ave(star$small, star$schid)
star$m_aide <- ave(star$aide, star$schid)
star$m_tchexper <- ave(star$tchexper, star$schid)
star$m_boy <- ave(star$boy, star$schid)
star$m_white_asian <- ave(star$white_asian, star$schid)
star$m_freelunch <- ave(star$freelunch, star$schid)

pdata_m <- pdata.frame(star, index = c("schid"))

pdata_clean <- na.omit(pdata_m)

mundlak_re <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                    m_small + m_aide + m_tchexper + m_boy + m_white_asian + m_freelunch, 
                  data = pdata_clean, model = "random")
summary(mundlak_re)

linearHypothesis(mundlak_re, c(
  "m_small = 0",
  "m_aide = 0",
  "m_tchexper = 0",
  "m_boy = 0",
  "m_white_asian = 0",
  "m_freelunch = 0"), vcov. = vcovHC(mundlak_re, type = "HC1", cluster = "group"))
