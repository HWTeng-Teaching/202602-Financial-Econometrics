load("C:/Users/user/Desktop/star5_small (1).rdata")

# --- (a) 小班對綜合成績的影響 ---
data_a <- subset(star5_small, small == 1 | regular == 1)
model_a <- lm(totalscore ~ small, data = data_a)
cat("\n[ (a) 小班 vs 普通班：總分差距 ]\n")
print(summary(model_a))

# --- (b) 分別看閱讀與數學的影響 ---
model_b_read <- lm(readscore ~ small, data = data_a)
model_b_math <- lm(mathscore ~ small, data = data_a)
cat("\n[ (b) 小班 vs 普通班：閱讀與數學分別差距 ]\n")
print(summary(model_b_read))
print(summary(model_b_math))

# --- (c) 助教對綜合成績的影響 ---
data_c <- subset(star5_small, aide == 1 | regular == 1)
model_c <- lm(totalscore ~ aide, data = data_c)
cat("\n[ (c) 助教班 vs 普通班：總分差距 ]\n")
print(summary(model_c))

# --- (d) 分別看閱讀與數學的影響 ---
model_d_read <- lm(readscore ~ aide, data = data_c)
model_d_math <- lm(mathscore ~ aide, data = data_c)
cat("\n[ (d) 助教班 vs 普通班：閱讀與數學分別差距 ]\n")
print(summary(model_d_read))
print(summary(model_d_math))
