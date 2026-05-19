load("D:/碩一下/計量經濟/作業/HW0512/15.20/star.rdata")
names(star)

#a
model_star =lm(readscore ~ small + aide + tchexper +
                   boy + white_asian + freelunch,
                 data = star)

summary(model_star)

#b
model_fe=lm(readscore ~ small + aide + tchexper +
                 boy + white_asian + freelunch +
                 factor(schid),
               data = star)

summary(model_fe)

#c
anova(model_star, model_fe)

#d
library(plm)

model_re=plm(readscore ~ small + aide + tchexper +
                  boy + white_asian + freelunch,
                data = star,
                index = c("schid", "id"),
                model = "random")

summary(model_re)
plmtest(model_re, type = "bp")

#f
library(dplyr)

star_mean=star %>%
  group_by(schid) %>%
  summarise(
    small_bar = mean(small, na.rm = TRUE),
    aide_bar = mean(aide, na.rm = TRUE),
    tchexper_bar = mean(tchexper, na.rm = TRUE),
    boy_bar = mean(boy, na.rm = TRUE),
    white_asian_bar = mean(white_asian, na.rm = TRUE),
    freelunch_bar = mean(freelunch, na.rm = TRUE)
  )

star2 = merge(star, star_mean, by = "schid")

mundlak_model = lm(readscore ~ small + aide + tchexper +
                      boy + white_asian + freelunch +
                      small_bar + aide_bar + tchexper_bar +
                      boy_bar + white_asian_bar + freelunch_bar,
                    data = star2)

summary(mundlak_model)
