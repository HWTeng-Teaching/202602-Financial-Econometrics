rm(list=ls())
#install.packages("plm")
library(POE5Rdata)
library(plm)
library(car)
library(lmtest)
library(sandwich)
data(star)
#str(star)
#15.20(a)
ols_mod <- lm(
  readscore ~ small + aide + tchexper +
    boy + white_asian + freelunch,
  data = star
)

coeftest(
  ols_mod,
  vcov = vcovHC(ols_mod, type = "HC1")
)

#15.20(b)
fe_mod <- lm(
  readscore ~ small + aide + tchexper +
    boy + white_asian + freelunch +
    factor(schid),
  data = star
)

coeftest(
  fe_mod,
  vcov = vcovHC(fe_mod, type = "HC1")
)[1:7, ]

#15.20(c)
linearHypothesis(
  fe_mod,
  grep("factor\\(schid\\)", names(coef(fe_mod)), value = TRUE),
  vcov = vcovHC(fe_mod, type = "HC1")
)

#15.20(d)
star$obs <- ave(star$readscore, star$schid, FUN = seq_along)

star_p <- pdata.frame(
  star,
  index = c("schid", "obs")
)

pool_mod <- plm(
  readscore ~ small + aide + tchexper +
    boy + white_asian + freelunch,
  data = star_p,
  model = "pooling"
)

re_mod <- plm(
  readscore ~ small + aide + tchexper +
    boy + white_asian + freelunch,
  data = star_p,
  model = "random"
)

coeftest(
  re_mod,
  vcov = vcovHC(re_mod, type = "HC1")
)

# LM test for random effects
plmtest(pool_mod, type = "bp")

#15.20(e)
fe_plm <- plm(
  readscore ~ small + aide + tchexper +
    boy + white_asian + freelunch,
  data = star_p,
  model = "within"
)

phtest(fe_plm, re_mod)
# 如果只看 BOY 的 FE vs RE 差異
coef(fe_plm)["boy"]
coef(re_mod)["boy"]

#15.20(f)
star$m_small <- ave(star$small, star$schid, FUN = mean)
star$m_aide <- ave(star$aide, star$schid, FUN = mean)
star$m_tchexper <- ave(star$tchexper, star$schid, FUN = mean)
star$m_boy <- ave(star$boy, star$schid, FUN = mean)
star$m_white_asian <- ave(star$white_asian, star$schid, FUN = mean)
star$m_freelunch <- ave(star$freelunch, star$schid, FUN = mean)

# Mundlak model
mundlak_mod <- lm(
  readscore ~ small + aide + tchexper +
    boy + white_asian + freelunch +
    m_small + m_aide + m_tchexper +
    m_boy + m_white_asian + m_freelunch,
  data = star
)

# 看係數
coeftest(
  mundlak_mod,
  vcov = vcovCL(mundlak_mod, cluster = ~ schid)
)

# Mundlak test:
# H0: school averages jointly insignificant
linearHypothesis(
  mundlak_mod,
  c(
    "m_small = 0",
    "m_aide = 0",
    "m_tchexper = 0",
    "m_boy = 0",
    "m_white_asian = 0",
    "m_freelunch = 0"
  ),
  vcov = vcovCL(mundlak_mod, cluster = ~ schid)
)