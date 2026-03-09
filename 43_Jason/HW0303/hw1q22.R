# Chan Nok Hang 414707007
# Q22
library(POE5Rdata)
data(star5_small)

# part a
star_small <- subset(star5_small, small == 1 | regular == 1)
model_small_total <- lm(totalscore ~ small, data = star_small)
summary(model_small_total)

# part b
model_small_read <- lm(readscore ~ small, data = star_small)
summary(model_small_read)

model_small_math <- lm(mathscore ~ small, data = star_small)
summary(model_small_math)

# part c
star_aide <- subset(star5_small, aide == 1 | regular == 1)
model_aide_total <- lm(totalscore ~ aide, data = star_aide)
summary(model_aide_total)

# part d
model_aide_read <- lm(readscore ~ aide, data = star_aide)
summary(model_aide_read)

model_aide_math <- lm(mathscore ~ aide, data = star_aide)
summary(model_aide_math)

