library(AER)
library(car)
library(stargazer)

library(POE5Rdata)
data("fultonfish")

#11.24(a)
mod_a_rf = lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
stargazer(mod_a_rf, type = "text", title = "Reduced-Form Equation for lprice")

pval_mixed = summary(mod_a_rf)$coefficients["mixed", 4]

f_test_a = linearHypothesis(mod_a_rf, c("stormy = 0", "mixed = 0"))
f_stat_a = f_test_a$F[2]

table_a = data.frame(Variable_Test = c("MIXED (p-value)", "Joint F-stat (STORMY, MIXED)"),
                     Value = c(pval_mixed, f_stat_a))
stargazer(table_a, type = "text", summary = FALSE, title = "Significance of Instruments for Demand")

# 11.24(b)
#內生變數: lprice
#自己的外生變數: mon, tue, wed, thu
#工具變數: stormy, mixed
mod_b_iv = ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy + mixed, data = fultonfish)
stargazer(mod_b_iv, type = "text", title = "IV Estimation of Demand Equation")

# 11.24(c)
iv_diag_c = summary(mod_b_iv, diagnostics = TRUE)$diagnostics
sargan_pval_c = iv_diag_c["Sargan", "p-value"]

table_c = data.frame(Test = "Sargan (Overidentification)", p_value = sargan_pval_c)
stargazer(table_c, type = "text", summary = FALSE, title = "Surplus Instrument Validity")

# 11.24(d)
f_test_d = linearHypothesis(mod_a_rf, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))
f_stat_d = f_test_d$F[2]
pval_d = f_test_d$`Pr(>F)`[2]

table_d = data.frame(Test = "Joint F-test (MON to THU)", F_statistic = f_stat_d, p_value = pval_d)
stargazer(table_d, type = "text", summary = FALSE, title = "Significance of Instruments for Supply")


data("truffles")

# 11.28(b)
#內生變數: q、工具變數: pf (供給面的外生變數)
iv_dem = ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)

# 內生變數: q、工具變數: ps, di (需求面的外生變數)
iv_sup = ivreg(p ~ q + pf | ps + di + pf, data = truffles)

stargazer(iv_dem, iv_sup, type = "text", 
          title = "2SLS Estimates for Inverse Demand and Supply",
          column.labels = c("Inverse Demand", "Inverse Supply"))

# 11.28(c)
gamma_2 = coef(iv_dem)["q"]
mean_p = mean(truffles$p)
mean_q = mean(truffles$q)

elas_d = (1 / gamma_2) * (mean_p / mean_q)

table_c = data.frame(Item = "Price Elasticity of Demand (at means)", Value = elas_d)
stargazer(table_c, type = "text", summary = FALSE)

# 11.28(d)
ps_val = 22; di_val = 3.5; pf_val = 23

# 計算給定外生變數後的截距項
int_dem = coef(iv_dem)["(Intercept)"] + coef(iv_dem)["ps"]*ps_val + coef(iv_dem)["di"]*di_val
slope_dem = coef(iv_dem)["q"]

int_sup = coef(iv_sup)["(Intercept)"] + coef(iv_sup)["pf"]*pf_val
slope_sup = coef(iv_sup)["q"]

# 繪圖
plot(NULL, xlim=c(0, 40), ylim=c(0, 100), xlab="Quantity (Q)", ylab="Price (P)", 
     main="Truffle Market Equilibrium (2SLS Estimates)")
abline(a=int_dem, b=slope_dem, col="steelblue", lwd=2) 
abline(a=int_sup, b=slope_sup, col="red", lwd=2)  
legend("topright", legend=c("Demand", "Supply"), col=c("steelblue", "red"), lwd=2)

# 11.28(e)
# P = int_dem + slope_dem * Q  ==  int_sup + slope_sup * Q
q_eq_iv = (int_dem - int_sup) / (slope_sup - slope_dem)
p_eq_iv = int_sup + slope_sup * q_eq_iv

rf_p = lm(p ~ ps + di + pf, data = truffles)
rf_q = lm(q ~ ps + di + pf, data = truffles)

newdata_eq = data.frame(ps = ps_val, di = di_val, pf = pf_val)
p_eq_rf = predict(rf_p, newdata = newdata_eq)
q_eq_rf = predict(rf_q, newdata = newdata_eq)

table_e = data.frame(Method = c("2SLS Structural", "Reduced-Form"),
                     Equilibrium_Q = c(q_eq_iv, q_eq_rf),
                     Equilibrium_P = c(p_eq_iv, p_eq_rf))
stargazer(table_e, type = "text", summary = FALSE, title = "Equilibrium Comparison")

# 11.28(f)
S