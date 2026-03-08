
# 2.22 -----------------
# install_github("ccolonescu/PoEdata")
rm(list=ls()) # Caution: this clears the Environment

# library(remotes)
install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
library(stargazer)

data('star5_small')

# a
df_a <- star5_small[star5_small$small == 1 | star5_small$regular == 1, ]

mod1 <- lm(totalscore ~ small, data = df_a)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
smod1 <- summary(mod1)
smod1
cat(b1,b2)

# b
mod2 <- lm(readscore ~ small, data = df_a)
b1 <- coef(mod2)[[1]]
b2 <- coef(mod2)[[2]]
cat(b1,b2)

mod3 <- lm(mathscore ~ small, data = df_a)
b1 <- coef(mod3)[[1]]
b2 <- coef(mod3)[[2]]
cat(b1,b2)


# c

df_c <- star5_small[star5_small$aide == 1 | star5_small$regular == 1, ]

mod1 <- lm(totalscore ~ aide, data = df_c)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
smod1 <- summary(mod1)
smod1
cat(b1,b2)


# d
mod1 <- lm(readscore ~ aide, data = df_c)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat(b1,b2)

mod1 <- lm(mathscore ~ aide, data = df_c)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat(b1,b2)


# 2.25----------------------
data("cex5_small")
df <- cex5_small

# a
fa = df$foodaway

png("hist_foodaway.png")
hist(df$foodaway)
dev.off()

summary(df$foodaway)


# b
df_b1 = df[df$advanced==1,]
summary(df_b1$foodaway)

df_b2 = df[df$college==1,]
summary(df_b2$foodaway)

df_b3 = df[df$college==0 & df$advanced==0,]
summary(df_b3$foodaway)


#c
fa_log = log(df$foodaway)

png("hist_foodaway_log.png")
hist(fa_log)
dev.off()

summary(fa_log)

print(length(fa_log))



# d
df$fa_log = log(df$foodaway)
df_clear = df[is.finite(df$fa_log),]
mod <- lm(fa_log ~ income, data=df_clear)
b1 <- coef(mod)[[1]]
b2 <- coef(mod)[[2]]
cat(b1,b2)

#e
plot(df_clear$income, df_clear$fa_log, 
     xlab="income", 
     ylab="log(foodaway)", 
     type = "p")
abline(b1,b2)

#f

plot(df_clear$income, mod$residuals, 
     xlab="income", 
     ylab="residual", 
     type = "p")


