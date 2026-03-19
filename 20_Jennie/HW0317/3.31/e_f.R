load("D:/碩一下/計量經濟/作業/HW0317/31/tuna.rdata")

#e
tuna$PRICE1=100*tuna$apr1
model=lm(sal1~PRICE1,data=tuna)
mean_sal1=mean(tuna$sal1)
mean_apr1=mean(tuna$apr1)
mean_sal1
mean_apr1

beta2=coef(model)["PRICE1"]
beta2
beta_per_dollar=beta2*100 

elasticity=beta_per_dollar*(mean_apr1/mean_sal1)
elasticity

ci_beta=confint(model,"PRICE1",level=0.95)
ci_per_dollar=ci_beta*100  
ci_elasticity=ci_per_dollar*(mean_apr1/mean_sal1)
ci_elasticity

#f
se_beta2=summary(model)$coefficients["PRICE1","Std. Error"]

mean_price1=mean(tuna$PRICE1)
mean_sal1=mean(tuna$sal1)

se_elasticity=se_beta2*(mean_price1/mean_sal1)
se_elasticity

df=50

t_stat=(elasticity+3)/se_elasticity
p_value=2*(1 - pt(abs(t_stat), df))
t_crit=qt(0.95, df)

t_stat
p_value
t_crit
