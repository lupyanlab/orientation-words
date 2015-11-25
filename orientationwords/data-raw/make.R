library(devtools)
library(dplyr)

load_all()

make_unilateral <- function(overwrite = FALSE) {
  unilateral_dir <- "data-raw/unilateral/"

  make_unilateral_version <- function(version) {
    regex_keys <- list("MOW1*", "MOW3*")
    compile(unilateral_dir, regex_keys[[version]]) %>%
      clean %>% recode %>% mutate(version = version)
  }

  unilateral <- plyr::rbind.fill(
    make_unilateral_version(1),
    make_unilateral_version(2)
  )

  use_data(unilateral, overwrite = overwrite)
}

make_bilateral <- function(overwrite = FALSE) {
  bilateral <- compile("data-raw/bilateral/") %>%
    clean %>% recode

  use_data(bilateral, overwrite = overwrite)
}
