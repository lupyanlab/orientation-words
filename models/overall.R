library(dplyr)
library(lme4)
library(broom)
library(ggplot2)

library(devtools)
load_all("orientationwords")

orientationwords <- compile("experiment/data/") %>%
  clean %>%
  recode

# ---- overall-mod
overall <- lmer(rt ~ cue_c * mask_c * response_c + (1|subj_id),
                data = orientationwords)
tidy(overall, effects = "fixed")

# ---- overall-plot
ggplot(orientationwords, aes(x = mask_c, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_type +
  base_theme
