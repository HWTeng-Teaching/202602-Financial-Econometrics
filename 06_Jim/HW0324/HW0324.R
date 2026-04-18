rm(list=ls())
library(POE5Rdata)
library(stargazer)
library(tseries)
library(ggplot2)
library(gridExtra)
# library(modelsummary)
data(collegetown)
?collegetown
#(a)
model_loglin <- lm(log(price) ~ sqft, data = collegetown)
summary(model_loglin)
stargazer(model_loglin)
sqft.mean = mean(collegetown$sqft)
price.mean = mean(collegetown$price)
0.036*sqft.mean
0.036*price.mean
exp(4.39)
# modelsummary(model_loglin, output = "latex")

#(b)
model_loglog <- lm(log(price) ~ log(sqft), data = collegetown)
stargazer(model_loglog)
1.025*price.mean/sqft.mean
exp(2.05)

#(c)
model_lin    <- lm(price ~ sqft, data = collegetown)
r2_lin <- summary(model_lin)$r.squared
gen_r2_a <- cor(collegetown$price, exp(fitted(model_loglin)))^2
gen_r2_b <- cor(collegetown$price, exp(fitted(model_loglog)))^2
c(r2_lin,gen_r2_a,gen_r2_b)

# --- (d) 殘差常態性檢定 (Jarque-Bera) ---
res_lin <- residuals(model_lin)
res_loglin <- residuals(model_loglin)
res_loglog <- residuals(model_loglog)
jb_lin <- jarque.bera.test(residuals(model_lin))
jb_loglin <- jarque.bera.test(residuals(model_loglin))
jb_loglog <- jarque.bera.test(residuals(model_loglog))
print(jb_lin)
print(jb_loglin)
print(jb_loglog)

p1 <- ggplot(data.frame(res = res_lin), aes(x = res)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(res_lin), sd = sd(res_lin)), color = "red") +
  ggtitle("Linear Model Residuals")

p2 <- ggplot(data.frame(res = res_loglin), aes(x = res)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(res_loglin), sd = sd(res_loglin)), color = "red") +
  ggtitle("Log-Linear Model Residuals")

p3 <- ggplot(data.frame(res = res_loglog), aes(x = res)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "salmon", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(res_loglog), sd = sd(res_loglog)), color = "red") +
  ggtitle("Log-Log Model Residuals")

grid.arrange(p1, p2, p3, ncol = 1)




#(e)
df_plot <- data.frame(
  SQFT = collegetown$sqft,
  Res_Lin = residuals(model_lin),
  Res_LogLin = residuals(model_loglin),
  Res_LogLog = residuals(model_loglog)
)


p1 <- ggplot(df_plot, aes(x = SQFT, y = Res_Lin)) +
  geom_point(alpha = 0.5, color = "skyblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Linear Model: Residuals vs SQFT") +
  theme_minimal()

p2 <- ggplot(df_plot, aes(x = SQFT, y = Res_LogLin)) +
  geom_point(alpha = 0.5, color = "lightgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Log-Linear Model: Residuals vs SQFT") +
  theme_minimal()

p3 <- ggplot(df_plot, aes(x = SQFT, y = Res_LogLog)) +
  geom_point(alpha = 0.5, color = "salmon") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Log-Log Model: Residuals vs SQFT") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 1)

# --- (f)(g) 預測 (2700 sqft) ---
new_data <- data.frame(sqft = 27)
pred_lin <- predict(model_lin, new_data, interval = "prediction")
pred_loglin <- exp(predict(model_loglin, new_data, interval = "prediction"))
pred_loglog <- exp(predict(model_loglog, new_data, interval = "prediction"))
