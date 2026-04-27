# (a)

N <- 1000       # 總樣本數
N_M <- 577      # 男性樣本數
N_F <- N - N_M  # 女性樣本數 
K <- 4          # 參數個數 (迴歸式中有截距項、EDUC、EXPER、METRO 共4個參數)

# 計算兩組的自由度 (df = N - K)
df_M <- N_M - K # 男性自由度 
df_F <- N_F - K # 女性自由度 

# 計算男性與女性的變異數估計值 (sigma^2)
SSE_M <- 97161.9174
sigma2_M <- SSE_M / df_M    # 男性變異數估計值 = SSE / df

sigma_F <- 12.024
sigma2_F <- sigma_F^2       # 女性變異數估計值

# 計算 GQ 檢定的 F 統計量
fstat <- sigma2_M / sigma2_F

# 尋找 F 分配的臨界值 (顯著水準 alpha = 0.05，雙尾檢定)
alpha <- 0.05
Flc <- qf(alpha/2, df_M, df_F)       # Lower critical F
Fuc <- qf(1 - alpha/2, df_M, df_F)   # Upper critical F

# 輸出最終結果
cat("Male Variance (sigma2_M):", sigma2_M, "\n")
cat("Female Variance (sigma2_F):", sigma2_F, "\n")
cat("F-statistic:", fstat, "\n")
cat("Rejection Region: F <", Flc, "or F >", Fuc, "\n")

# (補充) 計算 p-value 供雙重確認
pval <- 2 * (1 - pf(fstat, df_M, df_F))
cat("P-value:", pval, "\n")


# (b)

K <- 5

# 設定已婚組數據
N_M <- 600
SSE_M <- 100703.0471
df_M <- N_M - K
sigma2_M <- SSE_M / df_M

# 設定單身組數據
N_S <- 400
SSE_S <- 56231.0382
df_S <- N_S - K
sigma2_S <- SSE_S / df_S

# 計算 F 統計量 (大變異數放分子)
fstat <- sigma2_M / sigma2_S

# 尋找 F 分配的右尾臨界值 
alpha <- 0.05
F_critical <- qf(1 - alpha, df_M, df_S)

# 計算 p-value 
pval <- 1 - pf(fstat, df_M, df_S)

# 輸出結果
cat("Married Variance (sigma2_M):", sigma2_M, "\n")
cat("Single Variance (sigma2_S):", sigma2_S, "\n")
cat("F-statistic:", fstat, "\n")
cat("Rejection Region: F >", F_critical, "\n")
cat("P-value:", pval, "\n")

# 判斷結論
if(fstat > F_critical) {
  cat("Conclusion: Reject H0. Married individuals have more variable wages.\n")
} else {
  cat("Conclusion: Do not reject H0. Not enough evidence to show variance is different.\n")
}


# (c)

# 設定題目給定的已知參數
chi_stat <- 59.03
df <- 4           # Z 變數有 4 個: EDUC, EXPER, METRO, FEMALE
alpha <- 0.05

# 卡方分配的臨界值 (右尾檢定)
chi_critical <- qchisq(1 - alpha, df)

# p-value
pval <- 1 - pchisq(chi_stat, df)

# 輸出檢定結果
cat("Chi-square Statistic:", chi_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("Critical Value (5%):", chi_critical, "\n")
cat("P-value:", pval, "\n")

# 判斷結論
if(chi_stat > chi_critical) {
  cat("Conclusion: Reject H0. There is evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion: Do not reject H0. Homoskedasticity is assumed.\n")
}


# (d)

# 設定題目給定的檢定統計量與計算出的自由度
white_stat <- 78.82
df <- 12          # 4個原變數 + 2個平方項 + 6個交叉項
alpha <- 0.05

# 尋找卡方分配的臨界值 (右尾檢定)
chi_critical <- qchisq(1 - alpha, df)

# 計算 p-value
pval <- 1 - pchisq(white_stat, df)

# 輸出檢定結果
cat("White Test Statistic:", white_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("Critical Value (5%):", chi_critical, "\n")
cat("P-value:", pval, "\n")

# 判斷結論
if(white_stat > chi_critical) {
  cat("Conclusion: Reject H0. There is strong evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion: Do not reject H0. Homoskedasticity is assumed.\n")
}
