# ==========================================
# 財務計量經濟學 (Financial Econometrics) 
# Ch01: 課程前置環境建置與數據探索腳本
# ==========================================

# 1. 環境清理 (建議在開始新分析前執行)
rm(list=ls()) # 清除左側 Environment 的所有變數，避免舊資料干擾

# 2. 安裝與載入開發工具套件 (用於從 GitHub 安裝數據包)
if (!require("devtools")) {
  install.packages("devtools")
}
library(devtools)

# 3. 安裝課程專用數據包 PoEdata (Principles of Econometrics)
# 注意：這是 TA 在講義中特別修正過的安裝路徑
install_github("ccolonescu/PoEdata")
library(PoEdata)

# 4. 載入練習用的數據集 "andy"
data("andy")

# 5. 數據初步檢查
print("--- 數據前六筆 ---")
head(andy)      # 查看前 6 筆數據

print("--- 數據變數定義 ---")
# ?andy         # 這行會打開說明文件，手動執行即可

print("--- 數據總列數 ---")
nrow(andy)      # 查看觀測值總數

# 6. 變數選取 (根據講義範例將矩陣欄位拆分)
# 假設第一欄是應變數，二、三欄是自變數
v1 = andy[, 1]
v2 = andy[, 2]
v3 = andy[, 3]

# 7. 數據視覺化 (講義 Page 10 範例)
# 繪製各變數的圖形，觀察走勢與分佈
par(mfrow=c(2,2)) # 設定畫布為 2x2 格式，一次看四張圖

plot(v1, type="l", col="blue", main="Variable 1 (Time Series)")
plot(v2, type="l", col="red",  main="Variable 2 (Time Series)")
hist(v1, col="lightblue",      main="Histogram of V1")
boxplot(v1, col="lightgray",   main="Boxplot of V1")

# 重設畫布
par(mfrow=c(1,1))

# 8. 觀察變數間的相關性 (散佈圖)
pairs(andy, main="Scatter Plot Matrix for Andy Dataset")

# ==========================================
# 腳本結束。執行完畢後，請確認右下角 Plots 視窗是否有圖形產生。
# ==========================================