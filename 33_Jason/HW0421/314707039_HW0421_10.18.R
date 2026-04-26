library(PoEdata)
library(AER)    # 用於 ivreg()
data(mroz)
# 篩選出參與勞動市場的 428 位女性 (LFP=1)
mroz_working<-subset(mroz,lfp==1)
#a小題
mroz_working$mothercoll<-ifelse(mroz_working$mothereduc>12,1,0)
mroz_working$fathercoll<-ifelse(mroz_working$fathereduc>12,1,0)

p_mother <- mean(mroz_working$mothercoll) * 100
p_father <- mean(mroz_working$fathercoll) * 100
cat("母親有大學學歷比例:", p_mother, "%\n")
cat("父親有大學學歷比例:", p_father, "%\n")

#b小題
cor_matrix<-cor(mroz_working[,c("educ","mothercoll","fathercoll")])
print(cor_matrix)

#c小題
iv_model_c<-ivreg(log(wage)~exper+I(exper^2)+educ|
                    exper+I(exper^2)+mothercoll,data=mroz_working)
cat("IV 估計結果 (c):\n")
summary(iv_model_c)
cat("EDUC 係數的 95% 信心區間:\n")
confint(iv_model_c, "educ", level = 0.95)

#d小題
first_stage_d <- lm(educ ~ mothercoll + exper + I(exper^2), data = mroz_working)
summary(first_stage_d)
f_test_d<-linearHypothesis(first_stage_d,"mothercoll")
cat("\nmothercoll 的 F 檢定統計量:\n")
print(f_test_d)

#e小題
iv_model_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                      mothercoll + fathercoll + exper + I(exper^2), data = mroz_working)
cat("10.18 (e) 雙工具變數估計結果:\n")
summary(iv_model_e)
cat("\nEDUC 係數的 95% 信心區間:\n")
confint(iv_model_e, "educ", level = 0.95)

#f小題
first_stage_f <- lm(educ ~ mothercoll + fathercoll + exper + I(exper^2), data = mroz_working)

cat("\n10.18 (f) 第一階段迴歸結果:\n")
# 檢定 mothercoll 與 fathercoll 是否「同時」不為 0
f_test_f <- linearHypothesis(first_stage_f, c("mothercoll = 0", "fathercoll = 0"))
print(f_test_f)

#g小題
cat("\n10.18 (g) 工具變數有效性檢定 (查看 Sargan 統計量):\n")
summary(iv_model_e, diagnostics = TRUE)
