library(POE5Rdata)
library(ggplot2)
data("tuna")
?data("tuna")

#(a)
# SAL1
mean_SAL1 <- mean(tuna$sal1)
min_SAL1  <- min(tuna$sal1)
max_SAL1  <- max(tuna$sal1)
sd_SAL1   <- sd(tuna$sal1)

# APR1
mean_APR1 <- mean(tuna$apr1)
min_APR1  <- min(tuna$apr1)
max_APR1  <- max(tuna$apr1)
sd_APR1   <- sd(tuna$apr1)

# summary statistics
summary_table <- data.frame(
  Variable = c("SAL1", "APR1"),
  Mean = c(mean_SAL1, mean_APR1),
  Min = c(min_SAL1, min_APR1),
  Max = c(max_SAL1, max_APR1),
  SD = c(sd_SAL1, sd_APR1)
)
print(summary_table)

#plot
tuna$index <- 1:nrow(tuna)

ggplot(tuna, aes(x = index, y = sal1)) +
  geom_line(color = "purple", linetype = "solid", size = 1) +
  labs(title = "SAL1",
       x = "Week", y = "Sales") +
  theme_classic()

ggplot(tuna, aes(x = index, y = apr1)) +
  geom_line(color = "brown", linetype = "solid", size = 1) +
  labs(title = "APR1",
       x = "Week", y = "Price") +
  theme_classic()

#(b)
ggplot(tuna, aes(x = apr1, y = sal1)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "SAL1 vs APR1",
       x = "APR1",
       y = "SAL1") +
  theme_classic()

#(c)
SAL1 <- tuna$sal1
APR1 <- tuna$apr1
PRICE1 <- 100*APR1
linear_model <- lm(SAL1~PRICE1)
summary(linear_model)
sum_linear <- summary(linear_model)
b1 <- coef(sum_linear)[1]
b2 <- coef(sum_linear)[2]
se_b2 <-coef(sum_linear)[4]
df <- sum_linear$df[2]
tc <- qt(0.975,df)
lowb <- b2-tc*se_b2
upb <- b2+tc*se_b2
print(paste("lower bound=",lowb))
print(paste("upper bound=",upb))

#(d)
model <- lm(SAL1 ~ APR1, data = tuna)
#price=0.70
new_data <- data.frame(APR1 = 0.70)
# 90% CI
predict(model, newdata = new_data,
        interval = "confidence", level = 0.90)

#(e)
# mean
Pbar <- mean(APR1)
Qbar <- mean(SAL1)
b1 <- coef(model)["APR1"]

#elasticity at means
elasticity <- b1 * (Pbar / Qbar)
se_b1 <- summary(model)$coefficients["APR1", "Std. Error"]
se_elasticity <- se_b1 * (Pbar / Qbar)

#critical point
n <- length(SAL1)
tc_2 <- qt(0.975, df = n-2)

#CI
lowb_2 <- elasticity - tc_2 * se_elasticity
upb_2 <- elasticity + tc_2 * se_elasticity
print(paste("elatiscity=",elasticity))
print(paste("lower bound=",lowb_2))
print(paste("upper bound=",upb_2))

#(f)
#mean
Pbar <- mean(APR1)
Qbar <- mean(SAL1)
#estimate
b1 <- coef(model)["APR1"]
se_b1 <- summary(model)$coefficients["APR1", "Std. Error"]
# H0
b1_h0 <- -3 * (Qbar / Pbar)

#test
t_statistic <- (b1 - beta1_null) / se_b1
n <- length(SAL1)
df <- n-2
p_value <- 2 * (1 - pt(abs(t_stat), df))
tc_3 <- qt(0.95, df)

print(paste("t-statistic=", t_statistic))
print(paste("critical value=",tc_3))
print(paste("p-value=", p_value))