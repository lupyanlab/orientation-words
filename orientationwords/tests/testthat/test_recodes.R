library(testthat)
library(devtools)

library(dplyr)

load_all()

context("Recoders")

test_that("cue type works", {
  cue_types <- data_frame(cue_type = c("invalid", "valid")) %>%
    recode_cue_type

  expect_equal(cue_types$cue_c, c(-0.5, 0.5))

})

test_that("cue type works with empty strings", {
  cue_types <- data_frame(cue_type = c("invalid", "valid", "")) %>%
    recode_cue_type

  expect_equal(cue_types$cue_c, c(-0.5, 0.5, NA))
  expect_equal(cue_types$cue_task, c("invalid", "valid", "word"))
})
