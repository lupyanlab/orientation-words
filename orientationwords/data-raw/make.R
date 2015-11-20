library(devtools)
library(dplyr)

load_all()

unilateral <- compile("data-raw/unilateral/") %>%
  clean %>% recode

use_data(unilateral)
