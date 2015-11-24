# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)

library(devtools)
load_all("orientationwords")

data(unilateral)
unilateral$version <- 1

unilateral2 <- compile("experiment/data/", regex_key = "MOW3*") %>%
  clean %>%
  recode %>%
  mutate(version = 2)

unilateral <- plyr::rbind.fill(unilateral, unilateral2)

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

# ---- unilateral1-mod
unilateral1_mod <- lmer(rt ~ cue_c * mask_c * response_c + (1|subj_id),
                        data = filter(unilateral, version == 1))
tidy(unilateral1_mod, effects = "fixed")

# ---- error-mod
overall_error <- glmer(is_error ~ cue_c * mask_c * response_c + (1|subj_id),
                       data = filter(unilateral, version == 1),
                       family = binomial)
tidy(overall_error, effects = "fixed")

# ---- error-plot
ggplot(filter(unilateral, version == 1),
       aes(x = mask_c, y = is_error, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_cue_type +
  base_theme

# ---- error-type-plot
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

# ---- unilateral2-mod
unilateral2_mod <- lmer(rt ~ cue_c * mask_c * response_c + (1|subj_id),
                        data = filter(unilateral, version == 2))
tidy(unilateral2_mod, effects = "fixed")

# ---- unilateral1-plot
ggplot(filter(unilateral, version == 1),
       aes(x = mask_c, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_type +
  base_theme

# ---- unilateral2-plot
ggplot(filter(unilateral, version == 2),
       aes(x = mask_c, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_type +
  base_theme

# ---- unilateral-mod
unilateral_mod <- lmer(rt ~ cue_c * mask_c * response_c + (1|subj_id), data = unilateral)
tidy(unilateral_mod, effects = "fixed")

# ---- unilateral-plot
ggplot(unilateral, aes(x = mask_c, y = rt, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_rt +
  scale_color_cue_type +
  base_theme
