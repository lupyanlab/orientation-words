# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)

library(devtools)
load_all("orientationwords")

orientation <- compile("experiment/data") %>%
  clean %>% recode

z_score <- function(x) (x - mean(x))/sd(x)

subj_z <- orientation %>%
  group_by(subj_id) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error = mean(is_error, na.rm = TRUE)
  ) %>%
  transmute(
    subj_id,
    rt_rank_c = (rank(rt) - n()/2)/(n()/5),
    rt_z = z_score(rt),
    error_z = z_score(error)
  )

recode_measure <- function(frame) {
  measure_map <- data_frame(
    measure = c("rt_rank_c", "rt_z", "error_z"),
    measure_x = c(-0.1, 0.0, 1.0)
  )
  frame %>% left_join(measure_map)
}

rt_rank_levels <- subj_z %>% arrange(rt_z) %>% .$subj_id

subj_z_parallel <- subj_z %>%
  mutate(subj_id = factor(subj_id, levels = rt_rank_levels)) %>%
  gather(measure, z_score, -subj_id) %>%
  recode_measure

# ---- parallel
ggplot(subj_z_parallel, aes(x = measure_x, y = z_score, color = subj_id)) +
  geom_line(aes(group = subj_id)) +
  geom_text(aes(label = subj_id),
            data = filter(subj_z_parallel, measure == "rt_rank_c"),
            hjust = 1, size = 3) +
  scale_x_continuous("Measure", breaks = c(0, 1), labels = c("RT", "Error")) +
  coord_cartesian(ylim = c(-3.5, 3.5), xlim = c(-1, 2)) +
  theme(legend.position = "none")

# ---- tradeoff-calc
subj_z <- subj_z %>%
  mutate(
    tradeoff = rt_z - error_z,
    tradeoff_abs = abs(tradeoff)
  )

orientation <- orientation %>% left_join(subj_z)

# ---- correlation
cor_vars <- c("rt_z", "error_z", "tradeoff", "tradeoff_abs", "estimate")
cor(subjs_tradeoff[, cor_vars]) %>%
  fix_data_frame %>%
  select(term, cue_x_mask = estimate) %>%
  filter(term != "estimate")
  
# ---- tradeoff-mod
tradeoff_mod <- lmer(rt ~ (cue_c * mask_c) * tradeoff_abs + (1|subj_id),
                     data = orientation)
tidy(tradeoff_mod, effects = "fixed")

# ---- tradeoff-plot
subj_mods <- orientation %>%
  group_by(subj_id) %>%
  do(rt_mod = lm(rt ~ cue_c * mask_c, data = .)) %>%
  tidy("rt_mod") %>%
  filter(term == "cue_c:mask_c")

subjs_tradeoff <- subj_mods %>%
  ungroup() %>%
  left_join(subj_z) %>%
  mutate(subj_id = factor(subj_id, levels = rt_rank_levels))

ggplot(subjs_tradeoff, aes(x = tradeoff_abs, y = estimate)) +
  geom_point(aes(color = subj_id)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous("Speed-Accuracy Tradeoff (abs difference in RT and Error z-scores)") +
  scale_y_continuous("Cue x Mask Interaction (lm parameter estimate)") +
  theme(legend.position = "none")
