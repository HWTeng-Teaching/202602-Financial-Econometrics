
url <- "http://www.principlesofeconometrics.com/poe4/data/dat/fultonfish.dat"

# 讀取資料，並賦予欄位名稱
fultonfish <- read.table(url, header = FALSE)
colnames(fultonfish) <- c("date", "lprice", "quan", "lquan", "mon", "tue", "wed", "thu", 
                          "stormy", "mixed", "rainy", "cold", "totr", "diff", "change")

# 檢查前幾列
head(fultonfish)

library(AER)
library(car)

# 2. 核心分析：(a)Reduced-form
# 檢定天氣 (stormy, mixed) 是否會影響價格 (lprice)
model_rf <- lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = fultonfish)

cat("--- (a) 簡約式模型分析結果 ---\n")
print(summary(model_rf))

# 檢定 stormy 與 mixed 是否聯合顯著 (確認工具變數是否有效)
cat("\n(a) 天氣變數的聯合顯著性檢定：\n")
print(linearHypothesis(model_rf, c("stormy = 0", "mixed = 0")))


# 3.(b) & (c) 2SLS 需求函數與過度識別檢定
# 應變數: lquan, 內生變數: lprice, 工具變數: stormy, mixed
model_demand <- ivreg(lquan ~ lprice + mon + tue + wed + thu | 
                        mon + tue + wed + thu + stormy + mixed, 
                      data = fultonfish)

cat("\n--- (b) & (c) 2SLS 需求函數分析結果 ---\n")
# diagnostics = TRUE、 Part (c) SSargan test
print(summary(model_demand, diagnostics = TRUE))


# 4. 核心分析：(d) 檢定星期變數的影響
# 確認需求是否隨著星期一到星期四而有所不同
cat("\n--- (d) 星期變數的聯合顯著性檢定 ---\n")
print(linearHypothesis(model_rf, c("mon = 0", "tue = 0", "wed = 0", "thu = 0")))