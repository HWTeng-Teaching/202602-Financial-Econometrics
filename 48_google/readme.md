# 財務計量經濟學實作專案 (Financial Econometrics Homework)

- 本儲存庫用於存放 **財務計量經濟學** 課程之實作內容。
- 目錄結構採取模組化設計，以利於管理各階段的程式開發、統計分析與學術報告。

---

## 📂 目錄結構規畫

專案根目錄為 `48_google/`，後續新增作業請統一遵循以下層級：

```text
48_google/
└── homework/
    ├── hw{ID}/               # 作業序號 (例如: hw01, hw02...)
    │   ├── code/             # 程式實作目錄
    │   │   ├── *.py          # Python 分析腳本 (遵循 snake_case 規範)
    │   │   └── *.R           # R 語言統計腳本
    │   ├── latex/            # 學術報告與視覺化資源
    │   │   ├── *.tex         # LaTeX 原始檔 (報告排版)
    │   │   ├── *.png         # 統計圖表 (如: 殘差圖、擬合圖)
    │   │   └── *.ttf         # 報告指定字型檔
    │   └── docs/             # 原始題目文件 (例如: Exercise_*.pdf)
    └── readme.md             # 本說明文件
```

# 🛠 開發與排版規範
## 為了保持專案的專業性與一致性，請遵守以下約定：

## 1. 程式碼規範 (Coding Standards)
* Python: 變數、函式與模組命名必須使用 小寫蛇型 (snake_case)。

* 類別: 使用 PascalCase。

* 常數: 使用 SCREAMING_SNAKE_CASE。

* 縮寫: 類別名中若含縮寫（如 API）請保持大寫。

## 2. 報告撰寫 (Report Format)
* 所有正式報告均採用 LaTeX 撰寫，產出之 PDF 標題下方應包含：

1. title
2. name ( Jun-Gu Chen )
3. date

## 🚀 未來擴充指南
* 當有新的作業（例如 hw03）時，請執行以下步驟：

1. 在 homework/ 下新增 hw03/ 資料夾。

2. 建立 code/, latex/ 兩個子目錄並放入對應檔案。

* 未來在本 README.md 的「各階段作業摘要」區塊同步更新內容說明。
