setwd("G:/我的雲端硬碟/交大/碩一下/econometric/PoE5data")
load("star5_small.rdata")

# (a)
star_a <- subset(star5_small, aide == 0)
model_a <- lm(totalscore ~ small, data = star_a)
summary(model_a)

# (b) read vs math
summary(lm(readscore ~ small, data = star_a)) 
summary(lm(mathscore ~ small, data = star_a)) 

# (c) 助教
star_c <- subset(star5_small, small == 0)
model_c <- lm(totalscore ~ aide, data = star_c)
summary(model_c)

# (d) 
summary(lm(readscore ~ aide, data = star_c))
summary(lm(mathscore ~ aide, data = star_c))
