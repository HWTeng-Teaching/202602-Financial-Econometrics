rm(list=ls()) 
#ls()：列出目前 workspace 裡所有物件
#rm()：刪除物件
#rm(list = ls())：把所有物件刪掉
#problem 2.22
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/star5_small.rdata" 
#建立一個變數 url，內容是資料下載網址
dest <- file.path(tempdir(), "star5_small.rdata")
#tempdir()：取得 R 的暫存資料夾
#file.path()：建立完整檔案路徑
#dest = 暫存資料夾/star5_small.rdata(指定下載檔案存在哪裡)
download.file(url, dest, mode = "wb") 
#download.file(來源網址, 存放位置)
#mode = "wb"代表write binary(避免 Windows 下載 .rdata 時出現格式問題)
load(dest)
#load("star5_small.rdata")(把.rdata 檔案載入 R)

#(a)
idx <- star5_small$regular == 1 | star5_small$small == 1 
#取出資料框中的 regular 以及 small 的變數(==1表示是該類型的資料、|代表or)
#結論idx會是一個TRUE OR FALSE的向量，TRUE表示保留該筆資料
df <- star5_small[idx, ]
#資料框格式 -> data[列 , 欄]
#idx是篩選列 -> star5_small[idx, ]是只保留idx = TRUE的列(只包含regular & small且排除aide class)
mod1 <- lm(totalscore ~ small, data = df)
#totalscore ~ small指的是totalscorei = β0 + β1 * smalli + ui 
intercept <- coef(mod1)[1]
#取出截距；[1]表示第一個元素
#coef(mod1)會得到截距為β0且small的係數為β1
slope     <- coef(mod1)[2]
###########################
plot(df$small, df$totalscore, #前面為x軸，後面為y軸
     xlab="small", 
     ylab="totalscore",
     main = "Observations and  Fitted line", #圖片標題
     pch = 16,col = 'blue', #pch表示實心圓點
     type = "p") #p = point 只畫點
abline(intercept,slope ,col = 'red') #abline為畫一條直線，前兩者為參數
cat(
  "b1 is:", intercept,",when small=0,E[Totalscore|small=0]=b1", "\n",
  "b2 is:", slope,",when small increase 1(class is small),totalscore increase b2", "\n",
  "since b2>0,small class has higher totalscore,but the points in the two groups are widely dispersed.", "\n",
  "so it may not Statistically significant", "\n",
  sep = ""
)
#cat()把多個文字與變數 串接(concatenate) 起來並直接輸出到 console
#"\n" 代表換行
## b1 is:916.4417,when small=0,E[Totalscore|small=0]=b1
## b2 is:12.17533,when small increase 1(class is small),totalscore increase b2
## since b2>0,small class has higher totalscore,but the points in the two groups are widely dispersed.
## so it may not Statistically significant

#(b)
mod2 <- lm(readscore ~ small, data = df)
#β0=regularclass平均閱讀分數
#β1=smallclass與regularclass的平均差
intercept_2 <- coef(mod2)[1]
slope_2     <- coef(mod2)[2]


mod3 <- lm(mathscore ~ small, data = df)
intercept_3 <- coef(mod3)[1]
slope_3     <- coef(mod3)[2]
#β0=regularclass平均是學分數
#β1=smallclass與regularclass的平均差
###########################
par(mfrow = c(1, 2))
#par() = 設定圖形參數
#mfrow = c(1,2)意思是
#1 row
#2 columns
#也就是兩張圖並排 => [圖一] [圖二]
plot(df$small, df$readscore,  #畫閱讀成績點散圖
     xlab="small", 
     ylab="readscore",
     main = "small vs readscore",
     pch = 16,col = 'blue',
     type = "p") #圖形型態(只畫點)
abline(intercept_2,slope_2 ,col = 'red')

plot(df$small, df$mathscore, #畫數學成績點散圖
     xlab="small", 
     ylab="mathscore",
     main = "small vs mathscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_3,slope_3 ,col = 'red')

par(mfrow = c(1, 1)) # 恢復為單一圖形版面，之後再畫圖就不會出現並排圖

cat(
  "b1_read is:", intercept_2,".b1_math is:", intercept_3, "\n",
  "b2_read is:", slope_2,".b2_math is:", slope_3, "\n",
  "readscore is more improved than mathscore when class is small", "\n",
  "but it may not Statistically significant", "\n",
  sep = ""
)    
## b1_read is:432.665.b1_math is:483.7767
## b2_read is:6.924483.b2_math is:5.250849
## readscore is more improved than mathscore when class is small
## but it may not Statistically significant

#(c)
idx2 <- star5_small$regular == 1 | star5_small$aide == 1

df2 <- star5_small[idx2, ]
mod4 <- lm(totalscore ~ aide, data = df2)
intercept_4 <- coef(mod4)[1]
slope_4     <- coef(mod4)[2]

###########################
plot(df2$aide, df2$totalscore, 
     xlab="aide", 
     ylab="totalscore",
     main = "Observations and  Fitted line",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_4,slope_4 ,col = 'red')
cat(
  "b1 is: ", intercept_4,",when aide=0,E[Totalscore|aide=0]=b1", "\n",
  "b2 is: ", slope_4,",when aide increase 1(class has aide),totalscore increase b2", "\n",
  "since b2>0,regular class with aide has higher totalscore,but the points in the two groups are widely dispersed.", "\n",
  "so it may not Statistically significant", "\n",
  sep = ""
)    
## b1 is: 916.4417,when aide=0,E[Totalscore|aide=0]=b1
## b2 is: 4.306488,when aide increase 1(class has aide),totalscore increase b2
## since b2>0,regular class with aide has higher totalscore,but the points in the two groups are widely dispersed.
## so it may not Statistically significant

#(d)
mod5 <- lm(readscore ~ aide, data = df2)
intercept_5 <- coef(mod5)[1]
slope_5     <- coef(mod5)[2]

mod6 <- lm(mathscore ~ aide, data = df2)
intercept_6 <- coef(mod6)[1]
slope_6     <- coef(mod6)[2]

###########################
par(mfrow = c(1, 2))
plot(df2$aide, df2$readscore, 
     xlab="aide", 
     ylab="readscore",
     main = "aide vs readscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_5,slope_5 ,col = 'red')

plot(df2$aide, df2$mathscore, 
     xlab="aide", 
     ylab="mathscore",
     main = "aide vs mathscore",
     pch = 16,col = 'blue',
     type = "p")
abline(intercept_6,slope_6 ,col = 'red')
par(mfrow = c(1, 1))

cat(
  "b1_read is:", intercept_5,".b1_math is:", intercept_6, "\n",
  "b2_read is:", slope_5,".b2_math is:", slope_6, "\n",
  "readscore is more improved than mathscore when class has aide", "\n",
  "but it may not Statistically significant", "\n",
  sep = ""
)    
## b1_read is:432.665.b1_math is:483.7767
## b2_read is:2.871422.b2_math is:1.435066
## readscore is more improved than mathscore when class has aide
## but it may not Statistically significant
