library(lme4)
library(broom)
library(ggplot2)
library(devtools)
load_all("orientationwords")

orientation <- compile("experiment/data") %>%
  clean %>% recode

# ---- cueing-effect-mod
cueing_effect_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                          data = orientation)
tidy(cueing_effect_mod, effects = "fixed")

# ---- word-mod
word_mod <- lmer(rt ~ mask_c + (1|subj_id),
                 data = orientation)
tidy(word_mod, effects = "fixed")

# ---- overall-plot
ggplot(orientation, aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(aes(group = cue_task), stat = "summary", fun.y = "mean") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_task
