load("D:/碩一下/計量經濟/作業/HW0317/23/collegetown.rdata")

#a
model=lm(price~I(sqft^2),data=collegetown)
summary(model)
coef_table=summary(model)$coefficients

alpha2=coef_table["I(sqft^2)", "Estimate"]
se_alpha2=coef_table["I(sqft^2)", "Std. Error"]

t_stat=(alpha2-0.325)/se_alpha2
t_stat

df=df.residual(model)
t_critical=qt(0.95,df)
p_value=1-pt(t_stat, df)
t_critical
p_value

#b
t_stat2=(alpha2-0.1625)/se_alpha2
t_stat2

p_value2=1-pt(t_stat2, df)
p_value2
