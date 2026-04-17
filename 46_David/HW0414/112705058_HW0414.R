url <- "http://www.principlesofeconometrics.com/poe5/data/csv/vacation.csv"
df <- read.csv(url)

#16.a
model_a <- lm(miles ~ income + age + kids, data = df)

summary(model_a)
confint(model_a, "kids", level = 0.95)

#16.b

#residual
res <- residuals(model_a)


par(mfrow = c(1, 2))


plot(df$income, res, 
     main = "Residuals vs INCOME", 
     xlab = "Income ($1000s)", 
     ylab = "OLS Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2) 

plot(df$age, res, 
     main = "Residuals vs AGE", 
     xlab = "Average Age", 
     ylab = "OLS Residuals",
     pch = 20, col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))

#c

df_sorted <- df[order(df$income), ]

# data for Goldfeld–Quandt test
df_low <- df_sorted[1:90, ]
df_high <- df_sorted[(nrow(df_sorted) - 89):nrow(df_sorted), ]

# Two group
model_low <- lm(miles ~ income + age + kids, data = df_low)
model_high <- lm(miles ~ income + age + kids, data = df_high)

sse_low <- sum(residuals(model_low)^2)
sse_high <- sum(residuals(model_high)^2)

df1 <- 90 - 4  
df2 <- 90 - 4  

mse_low <- sse_low / df2
mse_high <- sse_high / df1

f_stat <- mse_high / mse_low

f_crit <- qf(p = 0.05, df1 = df1, df2 = df2, lower.tail = FALSE)

cat("高所得組 SSE:", sse_high, "\n")
cat("低所得組 SSE:", sse_low, "\n")
cat("F 檢定統計量:", f_stat, "\n")
cat("5% 臨界值:", f_crit, "\n")

#d
library(sandwich)
library(lmtest)

# 95% C.I. use Robust Standard Errors
robust_ci <- coefci(model_a, vcov = vcovHC(model_a), level = 0.95)

print("Robust 95% Confidence Intervals:")
print(robust_ci)

#e

#GLS
model_gls <- lm(miles ~ income + age + kids, data = df, weights = 1 / (income^2))

gls_ci <- confint(model_gls, "kids", level = 0.95)
robust_gls_ci <- coefci(model_gls, vcov = vcovHC(model_gls, type = "HC1"), level = 0.95)

cat("GLS 傳統信賴區間 (e):\n")
print(gls_ci)

cat("\nGLS 強健信賴區間 (e):\n")
print(robust_gls_ci["kids", ])
