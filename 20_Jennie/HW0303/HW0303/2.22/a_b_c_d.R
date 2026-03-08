load("D:/碩一下/計量經濟/作業/HW1/22/star5_small.rdata")
str(star5_small)

#a
star_reg_small=subset(star5_small, aide==0)
reg=lm(totalscore~small, data=star_reg_small)
summary(reg)

#b
reg_read=lm(readscore~small, data=star_reg_small)
summary(reg_read)

reg_math=lm(mathscore~small, data=star_reg_small)
summary(reg_math)

#c
star_reg_aide=subset(star5_small, small==0)
reg_aide=lm(totalscore~aide, data=star_reg_aide)
summary(reg_aide)

#d
reg_read_aide=lm(readscore~aide, data=star_reg_aide)
summary(reg_read_aide)

reg_math_aide=lm(mathscore~aide, data=star_reg_aide)
summary(reg_math_aide)
