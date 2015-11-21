library(dplyr)
library(tidyr)
library(lme4)
library(broom)
library(ggplot2)

library(devtools)
load_all("orientationwords")
data(unilateral)

# ---- error-mod
overall_error <- glmer(is_error ~ cue_c * mask_c * response_c + (1|subj_id),
                       data = unilateral,
                       family = binomial)
tidy(overall_error, effects = "fixed")

# ---- error-plot
ggplot(unilateral, aes(x = mask_c, y = is_error, color = cue_type)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("response_label") +
  scale_x_mask +
  scale_y_error +
  scale_color_cue_type +
  base_theme

# ---- error-type-plot
error_types <- unilateral %>%
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