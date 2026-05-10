library(PoEdata)
library(AER)  
library(car)   
data(fultonfish)

#a小題
model_a <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)
cat("11.24 (a) ln(PRICE) 簡化式迴歸結果:\n")
summary(model_a)

# 檢定 STORMY 與 MIXED 的聯合顯著性 (F-test)
cat("\nSTORMY 與 MIXED 的聯合顯著性檢定:\n")
f_test_a <- linearHypothesis(model_a, c("stormy = 0", "mixed = 0"))
print(f_test_a)

#b小題
model_b <- ivreg(lquan ~ lprice + mon + tue + wed + thu | 
                   mon + tue + wed + thu + stormy + mixed, data = fultonfish)
cat("\n11.24 (b) 需求方程 2SLS 估計結果:\n")
summary(model_b)

#c小題
cat("\n11.24 (c) Sargan 檢定 (查看過度識別有效性):\n")
summary(model_b, diagnostics = TRUE)

#d小題
cat("\n11.24 (d) 星期變數 (MON-THU) 對價格簡化式的聯合顯著性:\n")
f_test_d <- linearHypothesis(model_a, c("mon=0", "tue=0", "wed=0", "thu=0"))
print(f_test_d)
