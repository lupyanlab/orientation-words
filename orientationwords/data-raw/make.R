library(devtools)
library(dplyr)

load_all()

make_unilateral <- function(overwrite = FALSE) {
  unilateral <- compile("data-raw/unilateral/") %>%
    clean %>% recode

  use_data(unilateral, overwrite = overwrite)
}

make_bilateral <- function(overwrite = FALSE) {
  bilateral <- compile("data-raw/bilateral/") %>%
    clean %>% recode

  use_data(bilateral, overwrite = overwrite)
}
