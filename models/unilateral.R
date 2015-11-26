# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)

library(devtools)
load_all("orientationwords")

data(unilateral)

# Version 1 --------------------------------------------------------------------

# ---- v1-pic-mod
v1_pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                   data = filter(unilateral, version == 1, response_type == "pic"))
tidy(v1_pic_mod, effects = "fixed")

# ---- v1-pic-error-mod
v1_pic_error_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                          data = filter(unilateral, version == 1, response_type == "pic"),
                          family = binomial)
tidy(v1_pic_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- v1-word-mod
v1_word_mod <- lmer(rt ~ mask_c + cue_c + (1|subj_id),
                    data = filter(unilateral, version == 1, response_type == "word"))
tidy(v1_word_mod, effects = "fixed")

# ---- v1-word-error-mod
v1_word_error_mod <- glmer(is_error ~ mask_c + cue_c + (1|subj_id),
                           data = filter(unilateral, version == 1, response_type == "word"),
                           family = binomial)
tidy(v1_word_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- v1-plot
ggplot(filter(unilateral, version == 1),
       aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_unilateral_cue_task +
  base_theme

# ---- v1-error-plot
ggplot(filter(unilateral, version == 1),
       aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_unilateral_cue_task +
  base_theme

# ---- v1-error-type-plot
error_types <- unilateral %>%
  filter(version == 1) %>%
  group_by(response_type) %>%
  summarize(
    wrong_type = sum(error_type == "wrong_type", na.rm = TRUE)/n(),
    wrong_key = sum(error_type == "wrong_key", na.rm = TRUE)/n()
  ) %>%
  gather(error_type, error_rate, -response_type)

ggplot(error_types, aes(x = response_type, y = error_rate, fill = error_type, order = rev(error_type))) +
  geom_bar(stat = "identity") +
  scale_y_error +
  base_theme

# Version 2 --------------------------------------------------------------------

# ---- v2-pic-mod
v2_pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                   data = filter(unilateral, version == 2, response_type == "pic"))
tidy(v2_pic_mod, effects = "fixed")

# ---- v2-pic-error-mod
v2_pic_error_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                          data = filter(unilateral, version == 2, response_type == "pic"),
                          family = binomial)
tidy(v2_pic_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- v2-word-mod
v2_word_mod <- lmer(rt ~ mask_c + cue_c + (1|subj_id),
                    data = filter(unilateral, version == 2, response_type == "word"))
tidy(v2_word_mod, effects = "fixed")

# ---- v2-word-error-mod
v2_word_error_mod <- glmer(is_error ~ mask_c + cue_c + (1|subj_id),
                           data = filter(unilateral, version == 2, response_type == "word"),
                           family = binomial)
tidy(v2_word_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- v2-plot
ggplot(filter(unilateral, version == 2),
       aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_unilateral_cue_task +
  base_theme

# ---- v2-error-plot
ggplot(filter(unilateral, version == 2),
       aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_unilateral_cue_task +
  base_theme

# ---- v2-error-type-plot
error_types <- unilateral %>%
  filter(version == 2) %>%
  group_by(response_type) %>%
  summarize(
    wrong_type = sum(error_type == "wrong_type", na.rm = TRUE)/n(),
    wrong_key = sum(error_type == "wrong_key", na.rm = TRUE)/n()
  ) %>%
  gather(error_type, error_rate, -response_type)

ggplot(error_types, aes(x = response_type, y = error_rate, fill = error_type, order = rev(error_type))) +
  geom_bar(stat = "identity") +
  scale_y_error +
  base_theme

# Overall ----------------------------------------------------------------------

# ---- pic-mod
pic_mod <- lmer(rt ~ cue_c * mask_c + (1|subj_id),
                data = filter(unilateral, response_type == "pic"))
tidy(pic_mod, effects = "fixed")

# ---- pic-error-mod
pic_error_mod <- glmer(is_error ~ cue_c * mask_c + (1|subj_id),
                       data = filter(unilateral, response_type == "pic"),
                       family = binomial)
tidy(pic_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word-mod
word_mod <- lmer(rt ~ mask_c + cue_c + (1|subj_id),
                 data = filter(unilateral, response_type == "word"))
tidy(word_mod, effects = "fixed")

# ---- word-error-mod
word_error_mod <- glmer(is_error ~ mask_c + cue_c + (1|subj_id),
                        data = filter(unilateral, response_type == "word"),
                        family = binomial)
tidy(word_error_mod, effects = "fixed") %>%
  add_sig_stars

# ---- overall-plot
ggplot(unilateral, aes(x = mask_c, y = rt, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_unilateral_cue_task +
  base_theme

# ---- overall-error-plot
ggplot(unilateral, aes(x = mask_c, y = is_error, color = cue_task)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_unilateral_cue_task +
  base_theme

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