# ==========================================
# 1. 環境清理與套件自動配置
# ==========================================
rm(list = ls())
graphics.off()
cat("\014")

# 定義需要使用的套件
list_of_packages <- c("ggplot2", "dplyr", "broom", "stargazer")

# 自動檢查並安裝缺失套件
for (pkg in list_of_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ==========================================
# 2. 數據讀取與格式轉換
# ==========================================
data_url <- "http://www.principlesofeconometrics.com/poe5/data/csv/star5_small.csv"
star_df <- read.csv(data_url)

# 確保類別變數命名符合 snake_case
star_df <- star_df %>%
  dplyr::mutate(assignment_group = dplyr::case_when(
    small == 1 ~ "Small Class",
    aide == 1  ~ "Regular + Aide",
    regular == 1 ~ "Regular Class"
  ))

# ==========================================
# 3. 執行計量模型 (2.22 題目要求)
# ==========================================

# 篩選子樣本 (a, b)
df_small_vs_reg <- star_df %>% dplyr::filter(small == 1 | regular == 1)

# (a) 總分迴歸
model_small_total <- lm(totalscore ~ small, data = df_small_vs_reg)

# (b) 分科迴歸
model_small_read <- lm(readscore ~ small, data = df_small_vs_reg)
model_small_math <- lm(mathscore ~ small, data = df_small_vs_reg)

# 篩選子樣本 (c, d)
df_aide_vs_reg <- star_df %>% dplyr::filter(aide == 1 | regular == 1)

# (c) 助教效果迴歸
model_aide_total <- lm(totalscore ~ aide, data = df_aide_vs_reg)

# 輸出專業表格
stargazer(model_small_total, model_aide_total, 
          type = "text", 
          column.labels = c("Small Effect", "Aide Effect"),
          model.numbers = FALSE)

# ==========================================
# 4. 繪製精確係數圖表 (解決過時語法問題)
# ==========================================

# 整合繪圖數據
plot_data <- dplyr::bind_rows(
  broom::tidy(model_small_total, conf.int = TRUE) %>% 
    dplyr::filter(term == "small") %>% 
    dplyr::mutate(variable = "Total Score (Small Class)"),
  
  broom::tidy(model_aide_total, conf.int = TRUE) %>% 
    dplyr::filter(term == "aide") %>% 
    dplyr::mutate(variable = "Total Score (Teacher Aide)")
)

# 使用最新 ggplot2 語法繪圖
ggplot(plot_data, aes(x = estimate, y = variable)) +
  geom_point(size = 4, color = "#2c3e50") +
  # 修正：使用 geom_errorbar 並指定方向，解決 geom_errorbarh 過時問題
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#2c3e50") +
  # 修正：將 size 改為 linewidth
  geom_vline(xintercept = 0, linetype = "dashed", color = "#e74c3c", linewidth = 1) +
  theme_minimal() +
  labs(title = "Treatment Effects on Student Achievement",
       subtitle = "Comparison of Class Size Reduction vs. Teacher Aide Addition",
       x = "Estimated Points Increase",
       y = "Model Specification")

