# 414707002
#hw2 ch3-Q23

#part a
#安裝並載入讀取資料的套件
library(POE5Rdata)

#讀取數據
data("collegetown")

#幫助文件（Help）變數定義
?collegetown

#建立二次項模型
mod1 <- lm(price~I(sqft^2),data = collegetown)

#提取結果摘要
smod1<-summary(mod1)
smod1

#提取係數與標準誤差
a1<-coef(mod1)[1] #截距
a2<-coef(mod1)[2] #斜率
a2

sea1<-coef(smod1)[1,2] #截距標準誤
sea2<-coef(smod1)[2,2] #斜率標準誤
sea2

#計算測試統計量
tstar<-(40*a2-13)/(40*sea2)
tstar

alpha<-0.05
df=df.residual(mod1) 
tc<-qt(1-alpha,df) #t分配的百分位數函數
tc 

p<-1-pt(tstar,df) #右側尾巴面積
p

#part b
#計算測試統計量
tstar<-(80*a2-13)/(80*sea2) #邊際(微分)
tstar

alpha<-0.05
df=df.residual(mod1)
tc<-qt(1-alpha,df)
tc

#part c
x=2000/100 #SQFT 
a1
a2

#截距項的變異數
vara1<-vcov(mod1)[1,1] 
vara2<-vcov(mod1)[2,2]
#a1截距與斜率之間的共變異數
cova1a2<-vcov(mod1)[1,2]
L<-a1+a2*x^2
L

#線性組合變異數
varL=vara1+x^2*vara2+2*x*cova1a2

#計算標準誤差
seL<-sqrt(varL)  #變異數開根號
alpha<-0.05 
tcr<-qt(1-alpha/2,df)
lowbl<-L-tcr*seL #下限
upbl<-L+tcr*seL  #上限
lowbl

upbl


#part d
#找出SQFT為20的樣本
collegetown$price[which(collegetown$sqft==20)]
#計算平均價格
p20<-mean(collegetown$price[which(collegetown$sqft==20)])
p20

