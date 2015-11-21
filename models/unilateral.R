library(dplyr)
library(lme4)
library(broom)
library(ggplot2)

library(devtools)
load_all("orientationwords")
data(unilateral)

# ---- unilateral-mod
overall <- lmer(rt ~ cue_c * mask_c * response_c + (1|subj_id),
                data = unilateral)
tidy(overall, effects = "fixed")

# ---- unilateral-plot
ggplot(unilateral, aes(x = mask_c, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_type +
  base_theme
