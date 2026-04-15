# ==========================================
# (a) 檢定男女變異數是否相同 (雙尾檢定)
# ==========================================
df_M <- 577 - 3 - 1
df_F <- 1000 - 577 - 3 - 1

# 計算男性變異數 (SSE / df)
hat_sigma_M_squared <- 97161.9174 / df_M

# 女性變異數 (標準差的平方)
hat_sigma_F_squared <- 12.024^2

# F 檢定統計量
F_a <- hat_sigma_M_squared / hat_sigma_F_squared
print(paste("Part (a) F-statistic:", F_a))

# 計算雙尾臨界值 (alpha/2 = 0.025，取右尾)
alpha <- 0.05
# 寫法1：累積機率 0.975
critical_value_a <- qf(1 - alpha/2, df_M, df_F) 
print(paste("Part (a) Upper Critical Value:", critical_value_a))

# ==========================================
# (b) 檢定已婚變異數是否大於單身 (單尾檢定)
# ==========================================
df_SINGLE <- 400 - 4 - 1
df_MARRIED <- 600 - 4 - 1  # 總數 1000，已婚為 600

# 注意：題目給的是 SSE，必須除以 df 才是變異數 (MSE)
MSE_SINGLE <- 56231.0382 / df_SINGLE
MSE_MARRIED <- 100703.0471 / df_MARRIED

# F 檢定統計量 (把預期較大的 MARRIED 放分子)
F_statistic <- MSE_MARRIED / MSE_SINGLE
print(paste("Part (b) F-statistic:", F_statistic))

# 計算單尾臨界值 (alpha = 0.05，取右尾)
critical_value_b <- qf(1 - alpha, df_MARRIED, df_SINGLE)
print(paste("Part (b) Critical Value:", critical_value_b))