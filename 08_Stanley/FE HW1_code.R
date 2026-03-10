```{r}
# 2.22(a)
df1 <- star5_small[star5_small$aide == 0, ]
lm_a <- lm(totalscore ~ small, data = df1)
summary(lm_a)
```

```{r}
# 2.22(b)
lm_b1 <- lm(readscore ~ small, data = df1)
summary(lm_b1)
lm_b2 <- lm(mathscore ~ small, data = df1)
summary(lm_b2)
```

```{r}
# 2.22(c)
df2 <- star5_small[star5_small$small == 0, ]
lm_c <- lm(totalscore ~ aide, data = df2)
summary(lm_c)
```

```{r}
# 2.22(d)
lm_d1 <- lm(readscore ~ aide, data = df2)
summary(lm_d1)
lm_d2 <- lm(mathscore ~ aide, data = df2)
summary(lm_d2)
```

```{r}
# 2.25(a)
summary(cex5_small$foodaway)
hist(cex5_small$foodaway, main = "Histogram of FOODAWAY",
     xlab = "FOODAWAY", col = "cornflowerblue", border = "white")
```

```{r}
# 2.25(b)
summary(cex5_small$foodaway[cex5_small$advanced == 1])
summary(cex5_small$foodaway[cex5_small$college == 1])
summary(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0])
```

```{r}
# 2.25(c)
pos <- cex5_small$foodaway > 0
summary(log(cex5_small$foodaway[pos]))
hist(log(cex5_small$foodaway[pos]),
     main = "Histogram of ln(FOODAWAY)",
     xlab = "ln(FOODAWAY)", col = "mediumpurple", border = "white")
```

```{r}
# 2.25(d)
cex_d <- cex5_small[cex5_small$foodaway > 0, ]
lm_e <- lm(log(foodaway) ~ income, data = cex_d)
summary(lm_e)
```

```{r}
# 2.25(e)
plot(cex_d$income, log(cex_d$foodaway),
     main = "ln(FOODAWAY) vs INCOME",
     xlab = "Income ($100 units)",
     ylab = "ln(FOODAWAY)",
     pch = 1, col = "dodgerblue")
abline(lm_e, col = "tomato", lwd = 2)
```

```{r}
# 2.25(f)
r <- residuals(lm_e)
plot(cex_d$income, r,
     main = "Residuals vs INCOME",
     xlab = "Income ($100 units)",
     ylab = "Residuals",
     pch = 4, col = "darkorange")
abline(h = 0, col = "black", lty = 3)
```
