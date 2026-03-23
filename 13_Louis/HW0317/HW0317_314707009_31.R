#3.31
#(a.)
install.packages("ggplot2")
rm(list=ls())
library(ggplot2)
library(POE5Rdata)

data("tuna")

tuna <- cbind(week = 1:nrow(tuna), tuna)

summary_stats <- data.frame(
  Variable = c("SAL1", "APR1"),
  Mean = c(mean(tuna$sal1), mean(tuna$apr1)),
  Min = c(min(tuna$sal1), min(tuna$apr1)),
  Max = c(max(tuna$sal1), max(tuna$apr1)),
  SD = c(sd(tuna$sal1), sd(tuna$apr1))
)
print(summary_stats)

ggplot(tuna, aes(x = week, y = sal1)) +
  geom_line(color = "navy") +
  scale_y_continuous(breaks = seq(0, 40000, by = 10000)) +
  labs(title = "SAL1-WEEK Plot",  
       x = "Week", 
       y = "Number of cans sold") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))           

ggplot(tuna, aes(x = week, y = apr1)) +
  geom_line(color = "navy") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(title = "APR1-WEEK Plot",  
       x = "Week", 
       y = "Average price per can") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

cv_sales <- sd(tuna$sal1) / mean(tuna$sal1)
cv_price <- sd(tuna$apr1) / mean(tuna$apr1)

cat("銷量的 CV:", cv_sales, "\n")
cat("價格的 CV:", cv_price, "\n")

#(b.)
ggplot(tuna, aes(x = apr1, y = sal1)) +
  geom_point(color = "navy", alpha = 0.6) +               
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "SAL1-APR1 Plot",
       x = "Average price per can",
       y = "Number of cans sold") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))           

#(c.)
tuna$price1 <- 100 * tuna$apr1
model_c <- lm(sal1 ~ price1, data = tuna)

summary(model_c)

confint(model_c, "price1", level = 0.95)

#(d.)
new_data <- data.frame(price1 = 70)

expected_sales_ci <- predict(model_c, newdata = new_data, interval = "confidence", level = 0.90)

print(expected_sales_ci)

#(e.)
beta2_hat <- coef(model_c)["price1"]
beta2_ci <- confint(model_c, "price1", level = 0.95)

mean_p <- mean(tuna$price1)
mean_q <- mean(tuna$sal1)

multiplier <- mean_p / mean_q

elasticity_hat <- beta2_hat * multiplier
elasticity_ci <- beta2_ci * multiplier

cat(" 價格彈性估計結果\n",
    "點估計值 (Point Estimate):", elasticity_hat, "\n",
    "95% 信賴區間 (95% CI): [", elasticity_ci[1], ",", elasticity_ci[2], "]\n"
    )

#(f.)
alpha_e <- 0.1
se_beta2 <- summary(model_c)$coefficients["price1", "Std. Error"]

beta2_null <- -3 * (mean_q / mean_p)

t_e <- (beta2_hat - beta2_null) / se_beta2

df <- df.residual(model_c)
p_val_e <- 2 * pt(abs(t_e), df = df, lower.tail = FALSE)
tc_e <- qt(0.95, df)

cat("虛無假設 H0: elasticity = -3\n")
cat("對立假設 H1: elasticity ≠ -3\n")
cat("t 值為", t_e, "\n")
cat("拒絕域為絕對值 t 大於", tc_e, "\n")
cat("p-value:", p_val_e,"")

if (abs(t_e) > tc_e && p_val_e < alpha_e) {
  cat("結論：拒絕虛無假說 (Reject H0)。數據支持彈性係數應不等於3。\n")
} else {
  cat("結論：無法拒絕虛無假說 (Fail to reject H0)。\n")
}
