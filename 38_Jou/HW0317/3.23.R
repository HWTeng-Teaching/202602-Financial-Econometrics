url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
destfile <- tempfile(fileext = ".rdata")
download.file(url, destfile, mode = "wb")
load(destfile)

head(collegetown)
summary(collegetown)

collegetown$sqft2 <- collegetown$sqft^2
mod <- lm(price ~ sqft2, data = collegetown)

summary(mod)

a1 <- coef(mod)[1]
a2 <- coef(mod)[2]
se_a2 <- summary(mod)$coef["sqft2", "Std. Error"]
df_res <- df.residual(mod)

t95_right <- qt(0.95, df_res)     # 95% right-tail 
t95_two   <- qt(0.975, df_res)    # 95% two-tail 

# (a) ME = 40 * a2 
t_a <- (40 * a2 - 13) / (40 * se_a2)
p_val_a <- 1 - pt(t_a, df = 498)

cat("t-statistic:",t_a,"\n")
cat("rejection region: t >",t95_right,"\n")
cat(" p-value: ",p_val_a)

# (b) ME = 80 * a2 
t_b <- (80 * a2 - 13) / (80 * se_a2)
p_val_b <- 1 - pt(t_b, df = 498)

cat("t-statistic:",t_b,"\n")
cat("rejection region: t >",t95_right,"\n")
cat(" p-value: ",p_val_b)

# (c) Expected price at SQFT=20 and 95% CI
x0 <- 20
newd <- data.frame(sqft2 = x0^2)

ci_c <- predict(mod, newdata = newd, interval = "confidence",level=0.95 )  
print(ci_c)

# (d) sample mean price for sqft = 20
subset_20 <- subset(collegetown, sqft == 20)
n20 <- nrow(subset_20)
mean_price_20 <- mean(subset_20$price)

cat("number of houses: ",n20,"\n")
cat("selling prices:", subset_20$price,"\n")
cat("Sample Mean:", mean_price_20)

# Check compatibility with (c)
mean_price_20 >= ci_c[2] && mean_price_20 <= ci_c[3]