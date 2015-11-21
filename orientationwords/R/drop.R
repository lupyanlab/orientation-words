`%nin%` <- function(x, y) !(x %in% y)

OUTLIERS <- c("MOW212", "MOW205", "MOW216")

drop <- function(frame) {
  cat("dropping outliers:", OUTLIERS)
  frame %>% filter(subj_id %nin% OUTLIERS)
}