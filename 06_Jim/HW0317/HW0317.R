rm(list=ls())

# 18. 
# (a)
# y = 6.855+3.880x
library(ggplot2)
x_bar = 59.3
y_bar = 6.855+3.880*59.3
ggplot() +
geom_abline(intercept = 6.855, slope = 3.880, 
       col = "red",                                  
       lwd = 2) +
annotate("point", x = x_bar, y = y_bar, color = "darkgreen", size = 4, shape = 19) +
  annotate("text", x = x_bar+25, y = y_bar , 
           label = paste0(" (", x_bar, ", ", round(y_bar, 3), ")"), 
           color = "darkgreen", fontface = "bold") +
 xlim(0, 120) + ylim(0, 520)

# (b)
N =  20
df = N - 2
t_crit <- qt(0.975, df) # dt: pdf, pt: cdf, qt:Quantile(cdf_inverse, rt: 抽樣)

# 計算信心區間
margin_error <- t_crit * se_beta2
ci_lower <- beta2_hat - margin_error
ci_upper <- beta2_hat + margin_error

t_crit <- qt(0.995, df) 
qt(0.975, df)
qt(0.99, df)
qt(0.95, df)

# 23. 
library(POE5Rdata)
data(collegetown)
model = lm(price ~ I(sqft^2), data = collegetown)

summary(model)
t_stat <- (40*0.184519 - 13)/(40*0.005256)
p_value <- pt(t_stat, df = 498, lower.tail = FALSE)
t_crit <- qt(0.95, df = 500-2)
#b
t_stat <- (80*0.184519 - 13)/(80*0.005256)
p_value <- pt(t_stat, df = 498, lower.tail = FALSE)
t_crit <- qt(0.95, df = 500-2)
#c
new_data <- data.frame(sqft = 20)

pred_result <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)

# 輸出結果
print(pred_result)

v_matrix <- vcov(model)
print(v_matrix)

# 提取特定數值
var_alpha1 <- v_matrix[1, 1]        # 截距的變異數 (等於 Std. Error 的平方)
var_alpha2 <- v_matrix[2, 2]        # 斜率的變異數
cov_alpha1_alpha2 <- v_matrix[1, 2] # 兩者之間的共變異數
qt(0.975, df = 500-2)
library(tidyverse)
summary_at_20 = collegetown  |> 
  filter(sqft == 20) |> 
  summarize(
    count = n(),
    sample_mean = mean(price),
    sd_price = sd(price)
  )

print(summary_at_20)


#31. 
#a
data(tuna)
summary(tuna$sal1)
summary(tuna$apr1)
sd(tuna$sal1)
sd(tuna$apr1)

tuna <- tuna |> 
  mutate(week = row_number())
ggplot(tuna, aes(x = week, y = sal1)) +
  geom_line() + geom_point() +
  labs(title = "Weekly Sales of Brand 1 Tuna", y = "Unit Sales", x = "Week") +
  theme_minimal()

# 繪製價格隨時間變化圖
ggplot(tuna, aes(x = week, y = apr1)) +
  geom_line() + geom_point() +
  labs(title = "Weekly Price of Brand 1 Tuna", y = "Price per Can ($)", x = "Week") +
  theme_minimal()
#b
ggplot(tuna, aes(x = apr1, y = sal1)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  theme_minimal()

#c

tuna1 <- tuna  |> 
  mutate(price1 = 100 * apr1)
model_tuna <- lm(sal1 ~ price1, data = tuna1)
summary(model_tuna)
confint(model_tuna, "price1", level = 0.95)
#d
new_obs <- data.frame(price1 = 70)

predict(model_tuna, newdata = new_obs, interval = "confidence", level = 0.90)
mean(tuna1$sal1)
mean(tuna1$price1)
mean = -434.45 *78.25/6718.712
se = 78.58  *78.25/6718.712
tc = qt(0.975, df = 50-2)
mean-se*tc
mean+se*tc
tc = qt(0.95, df = 50-2)
2 * pt(-abs(-2.251), df = 50-2)
