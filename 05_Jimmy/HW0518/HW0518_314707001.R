rm(list=ls())
library(POE5Rdata)
data('star')
#a.

ols_model = lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

summary(ols_model)

#b.
library(plm)

star_panel = pdata.frame(star, index = c("schid", "id"))

fe_model = plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = star_panel, 
                model = "within")

summary(fe_model)

#c.
library(lmtest)

pooled_model = plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                    data = star_panel, 
                    model = "pooling")

fe_test = pFtest(fe_model, pooled_model)
print(fe_test)


#d.

re_model = plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = star_panel, 
                model = "random",
                random.method = "swar")

summary(re_model)

lm_test = plmtest(pooled_model, effect = "individual", type = "bp")
print(lm_test)
#e.

b_fe = coef(fe_model)
b_re = coef(re_model)[-1] # 扣除截距項
se_fe = sqrt(diag(vcov(fe_model)))
se_re = sqrt(diag(vcov(re_model)))[-1]

hausman_t = (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)
print(hausman_t)

#f.
library(car) 
star_clean = na.omit(star[, c("readscore", "small", "aide", "tchexper", 
                               "boy", "white_asian", "freelunch", "schid", "id")])

star_clean$m_small       = Between(star_clean$small,       star_clean$schid)
star_clean$m_aide        = Between(star_clean$aide,        star_clean$schid)
star_clean$m_tchexper    = Between(star_clean$tchexper,    star_clean$schid)
star_clean$m_boy         = Between(star_clean$boy,         star_clean$schid)
star_clean$m_white_asian = Between(star_clean$white_asian, star_clean$schid)
star_clean$m_freelunch   = Between(star_clean$freelunch,   star_clean$schid)

sp_m = pdata.frame(star_clean, index = c("schid", "id"))

m_pool = plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch + 
                m_small + m_aide + m_tchexper + m_boy + m_white_asian + m_freelunch, 
              data = sp_m, 
              model = "pooling")

mundlak_test = linearHypothesis(m_pool, 
                                 c("m_small=0", "m_aide=0", "m_tchexper=0", 
                                   "m_boy=0", "m_white_asian=0", "m_freelunch=0"), 
                                 vcov = vcovHC(m_pool, type="HC0", cluster="group"), 
                                 test = "Chisq")

print(mundlak_test)
