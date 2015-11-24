library(dplyr)
library(lme4)
library(broom)
library(ggplot2)

library(devtools)
load_all("orientationwords")
data(unilateral)

# ---- subjs
subjs <- unilateral %>%
  group_by(subj_id) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error = mean(is_error, na.rm = TRUE)
  ) %>%
  mutate(
    rank_rt = rank(rt),
    rank_error = rank(error)
  )
ggplot(subjs, aes(x = rank_error, y = error)) +
  geom_point() +
  geom_text(aes(label = subj_id), hjust = -0.1, angle = 90) +
  coord_cartesian(ylim = c(0, 0.30))

# ---- pic-mod
pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                data = filter(unilateral,
                              subj_id != "MOW111",
                              response_type == "pic"))
summary(pic_mod)

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
