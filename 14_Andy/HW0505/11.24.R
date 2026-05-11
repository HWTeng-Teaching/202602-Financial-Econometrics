# 1. 準備工作：載入套件與資料
rm(list = ls())
library(POE5Rdata)
library(AER)
library(car)

data("fultonfish")
dat <- fultonfish

# --- (a) 估計 ln(PRICE) 的簡化式 (Reduced-form) ---
# 簡化式是將內生變數對「系統中所有外生變數」進行回歸
# 除了原本的 stormy，現在多加入 mixed 變數
red_p <- lm(lprice ~ stormy + mixed + mon + tue + wed + thu, data = dat)

# 顯示結果：檢查 mixed 是否顯著
summary(red_p)

# 檢定 stormy 與 mixed 的聯合顯著性 (第一階段 F 檢定)
# 這是在確認這些「天氣變數」作為 IV 是否夠強
linearHypothesis(red_p, c("stormy = 0", "mixed = 0"))


# --- (b) 使用 2SLS 估計需求函數 ---
# 根據 (11.13)，需求函數包含：lprice (內生), mon, tue, wed, thu (外生)
# 我們使用 stormy 與 mixed 作為 lprice 的工具變數
fit_demand <- ivreg(lquan ~ lprice + mon + tue + wed + thu | 
                      mon + tue + wed + thu + stormy + mixed, data = dat)

# 顯示 2SLS 估計結果
summary(fit_demand)


# --- (c) Sargan 檢定 (過度識別檢定) ---
# 只有當 IV 數量 (2個: stormy, mixed) 大於內生變數數量 (1個: lprice) 時才能做
# 用來確認多出來的工具變數是否有效（外生性）
summary(fit_demand, diagnostics = TRUE)


# --- (d) 測試簡化式中星期變數的聯合顯著性 ---
# 這是為了看我們是否能反過來利用「時間變數」作為 IV 來估計「供給函數」
linearHypothesis(red_p, c("mon = 0", "tue = 0", "wed = 0", "thu = 0"))