library(stargazer)
library(tidyverse)
library(POE5Rdata)

data(collegetown)

# 23

alpha <- 0.05
model = lm(price ~ I(sqft^2), data = collegetown)
s_model <- summary(model)

se_a2 <- coef(s_model)[2, 2]
a1 <- coef(s_model)[[1]]
a2 <- coef(s_model)[[2]]

# a
t_stat <- (40 * a2 - 13) / (40 * se_a2)

df <- df.residual(model)
tc <- qt(1 - alpha, df)
p_value <- pt(t_stat, df, lower.tail = FALSE)

result <- data.frame(
  "值" = c(t_stat, tc, p_value, ifelse(p_value < 0.05, "拒絕 H0", "無法拒絕 H0")),
  row.names = c("t統計量", "臨界值", "p值", "結論")
)

stargazer(result, type='text', summary = FALSE)

# b
t_stat <- (80 * a2 - 13) / (80 * se_a2)

df <- df.residual(model)
tc <- qt(1 - alpha, df)
p_value <- pt(t_stat, df, lower.tail = FALSE)

result <- data.frame(
  "值" = c(t_stat, tc, p_value, ifelse(p_value < 0.05, "拒絕 H0", "無法拒絕 H0")),
  row.names = c("t統計量", "臨界值", "p值", "結論")
)

stargazer(result, type='text', summary = FALSE)

# c
expect_price <- a1 + a2 * 20^2
cov_matrix <- vcov(model)
se_price <- sqrt(
  cov_matrix[1, 1] + 20^2^2 * cov_matrix[2, 2] +
    2 * 20^2 * cov_matrix[1, 2]
)

tc <- qt(1 - alpha/2, df)
ci_lower <- expect_price - tc * se_price
ci_upper <- expect_price + tc * se_price

values <- c(
  "點估計值" = expect_price,
  "標準誤差" = se_price,
  "t 臨界值" = tc,
  "95% 下界" = ci_lower,
  "95% 上界" = ci_upper
)

result_df <- data.frame(值 = values)
stargazer(result_df, type='text', summary = FALSE)

# d
subdata <- collegetown |> filter(sqft == 20)
mean_price <- mean(subdata$price)
result_df <- data.frame(值 = c("mean price" = mean_price))
stargazer(result_df, type='text', summary = FALSE)


# 31
data(tuna)
# a
stargazer(
  tuna[, c("sal1", "apr1")],
  type = "text",
  summary.stat = c("mean", "min", "max", "sd")
)

plot(tuna$sal1, type = "o", col = "blue", 
     xlab = "Week", ylab = "Sales (SAL1)", main = "Weekly Sales Trend")

plot(tuna$apr1, type = "b", col = "red", 
     xlab = "Week", ylab = "Price (APR1)", main = "Weekly Price Trend")

# b
plot(tuna$sal1, tuna$apr1, col = "blue", 
     xlab = "Sales (SAL1)", ylab = "Price (APR1)", main = "Sales vs Price")
abline(lm(tuna$apr1 ~ tuna$sal1, data = tuna), col = "red", lwd = 2)

# c
tuna$price <- tuna$apr1 * 100
model_c <- lm(sal1 ~ price, data = tuna)
stargazer(
  model_c, 
  type = "text"
)
b2 <- coef(model_c)[[2]]
se_b2 <- sqrt(vcov(model_c)[2,2])

ci_b2 <- c(b2 - qt(0.975, df.residual(model_c)) * se_b2, 
           b2 + qt(0.975, df.residual(model_c)) * se_b2)

result_df <- data.frame(值 = ci_b2, row.names = c("lower bound", "upper bound"))
stargazer(result_df, type='text', summary = FALSE)

# d
new_data <- data.frame(price = 70)
predict_d <- predict(model_c, newdata = new_data, interval = "confidence", level = 0.90)

result_d <- data.frame(
  "值" = as.vector(predict_d),
  row.names = c("Predicted Sales", "90% Lower", "90% Upper")
)

stargazer(result_d, type='text', summary = FALSE)

# e
b2 <- coef(model_c)[["price"]]
se_b2 <- sqrt(vcov(model_c)["price", "price"])
avg_p <- mean(tuna$price)
avg_s <- mean(tuna$sal1)

elas_e <- b2 * (avg_p / avg_s)
se_elas_e <- se_b2 * (avg_p / avg_s)
tc_e <- qt(0.975, df.residual(model_c))

result_e <- data.frame(
  "值" = c(elas_e, elas_e - tc_e * se_elas_e, elas_e + tc_e * se_elas_e),
  row.names = c("es", "Lower", "Upper")
)

stargazer(result_e, type = "text", summary = FALSE)

# f
t_stat_f <- (elas_e - (-3)) / se_elas_e
tc_f <- qt(0.95, df.residual(model_c))
p_val_f <- 2 * pt(abs(t_stat_f), df.residual(model_c), lower.tail = FALSE)

result_f <- data.frame(
  "值" = c(t_stat_f, 
          tc_f, 
          p_val_f, 
          ifelse(p_val_f < 0.1, "拒絕 H0", "無法拒絕 H0")),
  row.names = c("t-stat", "tc", "p-value", "conclusion")
)

stargazer(result_f, type = "text", summary = FALSE)
