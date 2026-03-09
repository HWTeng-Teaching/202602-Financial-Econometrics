library(POE5Rdata)
mod1 <- lm(totalscore ~ small, data = star5_small)
smod1 <- summary(mod1)
smod1
mod2 <- lm(readscore ~ small, data = star5_small)
smod2 <- summary(mod2)
smod2
mod3 <- lm(mathscore ~ small, data = star5_small)
smod3 <- summary(mod3)
smod3

mod4 <- lm(totalscore ~ aide, data = star5_small)
smod4 <- summary(mod4)
smod4
mod5 <- lm(readscore ~ aide, data = star5_small)
smod5 <- summary(mod5)
smod5
mod6 <- lm(mathscore ~ aide, data = star5_small)
smod6 <- summary(mod6)
smod6