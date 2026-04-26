load("Documents/R/data_needed/poe5rdata/vacation.rdata")

# part a
alpha = 0.05
model_ols <- lm(miles~income+age+kids, data = vacation)
summary(model_ols)
ci <- confint(model_ols)
print(ci)
# -------------------------------------------------------

# part b
ehat <- resid(model_ols)
ggplot(vacation, aes(x = vacation$income, y = ehat))+
  geom_point()+
  labs(title = "Scatter plots of income and residuals",
       x = "income",
       y = "residuals")

ggplot(vacation, aes(x = vacation$age, y = ehat))+
  geom_point()+
  labs(title = "Scatter plots of age and residuals",
       x = "age",
       y = "residuals")
# -------------------------------------------------------

# part c
vacation_sorted <- vacation[order(vacation$income), ]

# first 90 observations
mod_first90 <- lm(miles~income+age+kids, data = vacation_sorted[1:90, ])
# last 90 observations
mod_last90 <- lm(miles~income+age+kids, data = vacation_sorted[111:200, ])

df1 <- mod_first90$df.residual
df2 <- mod_last90$df.residual

sigsquared1 <- glance(mod_first90)$sigma^2
sigsquared2 <- glance(mod_last90)$sigma^2
gq_fstat <- sigsquared2/sigsquared1

flc <- qf(alpha/2, df1, df2)
fuc <- qf(1-alpha/2, df1, df2)
cat(flc, fuc, gq_fstat)
# -------------------------------------------------------

# part d
# 計算穩健標準誤矩陣
cov1 <- hccm(model_ols, type = "hc1")
vacation.HC1 <- coeftest(model_ols, vcov.=cov1)

# 先存成 Data Frame 再畫表
result_dataframe <- tidy(vacation.HC1)
table <- kable(result_dataframe, caption = "Robust (HC1) standard errors")

# 從 result_dataframe 提取數字
b_kids <- result_dataframe$estimate[result_dataframe$term == "kids"]
se_robust <- result_dataframe$std.error[result_dataframe$term == "kids"]

tc <- qt(0.975, df = nobs(model_ols) - 4)
ci_robust <- c(b_kids - tc*se_robust, b_kids + tc*se_robust)
print(ci_robust)
# -------------------------------------------------------

# part e
w <- 1/vacation$income^2
model_gls <- lm(miles~income+age+kids, weights = w, data = vacation)
summary(model_gls)

# conventional GLS standard error
gls_result <- tidy(model_gls)
b_kids_gls <- gls_result$estimate[gls_result$term == "kids"]
se_conv_gls <- gls_result$std.error[gls_result$term == "kids"]
tc <- qt(0.975, df = df.residual(model_gls))
ci_conv_gls <- c(b_kids_gls - tc*se_conv_gls, b_kids_gls + tc*se_conv_gls)
print(ci_conv_gls)

# robust GLS standard error
# 計算穩健斜方差矩陣
cov2 <- hccm(model_gls, type = "hc1")
# 使用 coeftest 取得穩健檢定結果
rob_gls_result <- tidy(coeftest(model_gls, vcov.=cov2))
# 抓取穩健標準誤
se_robust_gls <- rob_gls_result$std.error[rob_gls_result$term == "kids"]
# 計算穩健信賴區間
ci_robust_gls <- c(b_kids_gls - tc*se_robust_gls, b_kids_gls + tc*se_robust_gls)
print(ci_robust_gls)



