library(dplyr)
library(ggplot2)

library(devtools)
load_all("orientationwords")
orientation <- compile("experiment/data") %>%
  clean

subjs <- orientation %>%
  group_by(subj_id) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error = mean(is_error, na.rm = TRUE)
  ) %>%
  mutate(
    rank_rt = rank(rt),
    rank_error = rank(error, ties.method = "first")
  )

rt_lim <- range(subjs$rt) %>% round(digits = -2) + c(-100, 300)
error_lim <- c(0, max(subjs$error) + 0.05)
rank_lim <- range(subjs$rank_rt) + c(-0.5, 0.5)

ggplot(subjs, aes(x = rank_rt, y = rt, color = subj_id, label = subj_id)) +
  geom_point() +
  geom_text(angle = 90, hjust = -0.1) +
  coord_cartesian(ylim = rt_lim, xlim = rank_lim) +
  scale_x_continuous("Rank", breaks = 1:nrow(subjs)) +
  scale_y_rt +
  base_theme +
  theme(legend.position = "none")
ggsave("descriptives/subjs-rt.png")

ggplot(subjs, aes(x = rank_error, y = error, color = subj_id, label = subj_id)) +
  geom_point() +
  geom_text(angle = 90, hjust = -0.1) +
  coord_cartesian(ylim = error_lim, xlim = rank_lim) +
  scale_x_continuous("Rank", breaks = 1:nrow(subjs)) +
  scale_y_error +
  base_theme +
  theme(legend.position = "none")
ggsave("descriptives/subjs-error.png")