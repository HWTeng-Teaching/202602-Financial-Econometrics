library(PoEdata)
library(AER)
data(capm4)
capm4$x<-capm4$mkt-capm4$riskfree
capm4$y<-capm4$msft-capm4$riskfree

#a小題
model_ols<-lm(y~x,data=capm4)
summary(model_ols)

#b小題
# 依照市場超額報酬 (x) 由小到大排序並賦予排名
capm4$rank<-rank(capm4$x)
first_stage_b <- lm(x ~ rank, data = capm4)
summary(first_stage_b)

#c小題
v_hat<-resid(first_stage_b)
model_augmented<-lm(y~x+v_hat,data=capm4)
summary(model_augmented)

#d小題
model_iv <- ivreg(y ~ x | rank, data = capm4)
summary(model_iv)
# 比較 OLS 與 IV 的係數
cat("OLS 估計的 Beta:", coef(model_ols)["x"], "\n")
cat("IV  估計的 Beta:", coef(model_iv)["x"], "\n")

#e小題
capm4$pos <- ifelse(capm4$x > 0, 1, 0)
first_stage_e <- lm(x ~ rank + pos, data = capm4)
summary(first_stage_e)

#f小題
v_hat_e <- resid(first_stage_e)
hausman_model <- lm(y ~ x + v_hat_e, data = capm4)
summary(hausman_model)

#g小題
model_iv_g<-ivreg(y~x|rank+pos,data=capm4)
summary(model_iv_g)

#h小題
e_hat_iv <- resid(model_iv_g)
sargan_reg <- lm(e_hat_iv ~ rank + pos, data = capm4)
n_obs <- nrow(capm4)
r2_sargan <- summary(sargan_reg)$r.squared
sargan_stat <- n_obs * r2_sargan
p_val_sargan <- 1 - pchisq(sargan_stat, df = 1)
cat("Sargan 統計量 (N*R2):", sargan_stat, "\n")
cat("p-value:", p_val_sargan, "\n")
