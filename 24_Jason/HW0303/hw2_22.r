library(devtools)
install_git("https://github.com/ccolonescu/POE5Rdata")
library(POE5Rdata)
data(star5_small)
df <- star5_small

# Part (a)
df_a <- subset(df, small == 1 | regular == 1)
reg_a <- lm(totalscore ~ small, data = df_a)
summary(reg_a)
coef(reg_a)[[1]]
coef(reg_a)[[2]]

# Part (b) 
reg_b1 <- lm(readscore ~ small, data = df_a)
summary(reg_b1)
coef(reg_b1)[[1]]
coef(reg_b1)[[2]]
reg_b2 <- lm(mathscore ~ small, data = df_a)
summary(reg_b2)
coef(reg_b2)[[1]]
coef(reg_b2)[[2]]

# Part (c)
df_c <- subset(df, regular == 1 | aide == 1)
reg_c <- lm(totalscore ~ aide, data = df_c)
summary(reg_c)
coef(reg_c)[[1]]
coef(reg_c)[[2]]

# Part (d)
reg_d1 <- lm(readscore ~ aide, data = df_c)
summary(reg_d1)
coef(reg_d1)[[1]]
coef(reg_d1)[[2]]
reg_d2 <- lm(mathscore ~ aide, data = df_c)
summary(reg_d2)
coef(reg_d2)[[1]]
coef(reg_d2)[[2]]