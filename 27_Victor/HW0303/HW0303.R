rm(list=ls()) 

library (devtools)
install_git("https://github.com/ccolonescu/POE5Rdata")
install.packages("stargazer")
install.packages("tidyverse")
library(stargazer)
library(tidyverse)
library(POE5Rdata)

data(star5_small)

# 2.22
# a
star_a =  star5_small |> filter(small == 1 | regular == 1)
model_a = lm(totalscore ~ small, data = star_a)
stargazer(model_a, 
          type = "html", 
          out = "star_a_reg.doc",
          title = "The Effect of Small Class Size on Total Scores",
          dep.var.labels = "Total Score",
          covariate.labels = "Small Class")
# b
model_b_1 = lm(readscore ~ small, data = star_a)
stargazer(model_b_1, 
          type = "html", 
          out = "star_b_1_reg.doc",
          title = "The Effect of Small Class Size on Reading Scores",
          dep.var.labels = "Read Score",
          covariate.labels = "Small Class")

model_b_2 = lm(mathscore ~ small, data = star_a)
stargazer(model_b_2, 
          type = "html", 
          out = "star_b_2_reg.doc",
          title = "The Effect of Small Class Size on Math Scores",
          dep.var.labels = "Math Score",
          covariate.labels = "Small Class")

# c
star_c =  star5_small |> filter(regular == 1)
model_c = lm(totalscore ~ aide, data = star_c)
stargazer(model_c, 
          type = "html", 
          out = "star_c_reg.doc",
          title = "The Effect of Aide on Total Scores",
          dep.var.labels = "Total Score",
          covariate.labels = "Aide")

# d
model_d_1 = lm(readscore ~ aide, data = star_c)
stargazer(model_d_1, 
          type = "html", 
          out = "star_d_1_reg.doc",
          title = "The Effect of Aide on Reading Scores",
          dep.var.labels = "Reading Score",
          covariate.labels = "Aide")

model_d_2 = lm(mathscore ~ aide, data = star_c)
stargazer(model_d_2, 
          type = "html", 
          out = "star_d_2_reg.doc",
          title = "The Effect of Aide on Math Scores",
          dep.var.labels = "Math Score",
          covariate.labels = "Aide")


# 2.25
# a
data("cex5_small")
hist(
  cex5_small$foodaway,
  main = "Hist of foodaway",
  xlab = "foodaway"
)
summary(cex5_small$foodaway)


# b
cex_1 = cex5_small |> filter(advanced == 1)
cex_2 = cex5_small |> filter(college == 1)
cex_3 = cex5_small |> filter(advanced == 0 & college == 0)
summary(cex_1$foodaway)
summary(cex_2$foodaway)
summary(cex_3$foodaway)

# c
cex5_small <- cex5_small |> filter(foodaway > 0)
cex5_small <- cex5_small %>%
  mutate(ln_foodaway = log(foodaway))
hist(
  cex5_small$ln_foodaway,
  main = "Hist of ln-foodaway",
  xlab = "ln-foodaway"
)

# d
model_ln = lm(ln_foodaway ~ income, data = cex5_small)
stargazer(model_ln, 
          type = "html", 
          out = "cex_ln_reg.doc",
          title = "The Effect of Foodaway on Income",
          dep.var.labels = "Ln Foodaway",
          covariate.labels = "Income")

# e
plot(ln_foodaway ~ income, data = cex5_small)
abline(model_ln, col = "red", lwd = 2)

# f
r =  resid(model_ln)
plot(cex5_small$income, r)
abline(h = 0, col = "red", lty = 2)
