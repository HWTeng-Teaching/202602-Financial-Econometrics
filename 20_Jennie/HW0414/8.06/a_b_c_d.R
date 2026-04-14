#a
SSE_M= 97161.9174
n_M=577
n_F=423
k=4

sigma2_M=SSE_M / (n_M - k)
sigma2_F=12.024^2

F_stat=sigma2_M/sigma2_F
upper=qf(0.975, df1 = n_M - k, df2 = n_F - k)
lower=qf(0.025, df1 = n_M - k, df2 = n_F - k)

F_stat
upper
lower

#b
SSE_single =56231.0382
SSE_married= 100703.0471
n_single= 400
n_married = 600

k2 = 5

sigma2_single=SSE_single / (n_single - k2)
sigma2_married=SSE_married / (n_married - k2)

F_stat2=sigma2_married / sigma2_single

df1= n_married - k2
df2= n_single - k2

F_critical2 =qf(0.95, df1, df2)
p_value = 1 - pf(F_stat, df1, df2)

F_stat2
F_critical2
p_value

#c
qchisq(0.95, df = 4)
p_value=1-pchisq(59.03, df = 4)
p_value

#d
qchisq(0.95, df = 12)
