#2.22
#a
sub_a <- subset(df_star, regular == 1 | small == 1)
mod_a <- lm(totalscore ~ small, data = sub_a)
summary(mod_a)

#b 
mod_b_read <- lm(readscore ~ small, data = sub_a)
mod_b_math <- lm(mathscore ~ small, data = sub_a)

summary(mod_b_read)
summary(mod_b_math)

#c
sub_c <- subset(df_star, regular == 1 | aide == 1)
mod_c <- lm(totalscore ~ aide, data = sub_c)
summary(mod_c)

#d
mod_d_read <- lm(readscore ~ aide, data = sub_c)
mod_d_math <- lm(mathscore ~ aide, data = sub_c)

summary(mod_d_read)
summary(mod_d_math)
