library(POE5Rdata)
data("star5_small")
?star5_small

(a)
# subset data
data_a <- subset(star5_small, small == 1 | regular== 1)
# regression
model_a <- lm(totalscore ~ small, data = data_a)
summary(model_a)

(b)
