library(POE5Rdata)
data("star5_small")
#開啟資料集的說明文件
?star5_small 

#(a)
# subset data
data_a <- subset(star5_small, small == 1 | regular== 1)
# regression
model_a <- lm(totalscore ~ small, data = data_a)
summary(model_a)

#(b)
# reading score
model_b1 <- lm(readscore ~ small, data = data_a)
summary(model_b1)

# math score
model_b2 <- lm(mathscore ~ small, data = data_a)
summary(model_b2)

#(c)
data_c <- subset(star5_small, regular == 1 | aide == 1)
model_c <- lm(totalscore ~ aide, data = data_c)
summary(model_c)

#(d)
# reading score
model_d1 <- lm(readscore ~ aide, data = data_c)
summary(model_d1)

# math score
model_d2 <- lm(mathscore ~ aide, data = data_c)
summary(model_d2)
