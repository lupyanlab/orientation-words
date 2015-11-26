library(testthat)
library(devtools)

library(dplyr)

load_all()

context("Cue type recodes")

test_that("cue type works", {
  cue_types <- data_frame(cue_type = c("invalid", "valid")) %>%
    recode_cue_type

  expect_equal(cue_types$cue_c, c(-0.5, 0.5))
})

test_that("cue type works with empty strings", {
  cue_types <- data_frame(cue_type = c("invalid", "valid", "", NA)) %>%
    recode_cue_type

  expect_equal(cue_types$cue_c, c(-0.5, 0.5, NA, NA))
  expect_equal(cue_types$cue_task, c("invalid", "valid", "word", "word"))
})

test_that("cue task is relabeled for unilateral word trials", {
  cue_types <- data_frame(
    cue_type = c("invalid", "valid"),
    response_type = c("word", "word")
  ) %>% recode_cue_type

  expect_equal(as.character(cue_types$cue_task), c("word", "word"))
})

test_that("cue task is ordered factor", {

  create_cue_types <- function(stringsAsFactors) {
    expand.grid(
      response_type = c("word", "pic"),
      cue_type = c("invalid", "valid"),
      stringsAsFactors = stringsAsFactors
    ) %>% recode_cue_type
  }

  expected_levels <- c("invalid", "valid", "word")

  cue_tasks_from_strings <- create_cue_types(FALSE)$cue_task
  cue_tasks_from_factor <- create_cue_types(TRUE)$cue_task

  expect_equal(levels(cue_tasks_from_strings), expected_levels)
  expect_equal(cue_tasks_from_strings, cue_tasks_from_factor)
})

context("Error type recodes")

test_that("error type works", {
  error_types <- expand.grid(
    response = c("left", "right", "same", "different"),
    correct_response = c("left", "right", "same", "different"),
    stringsAsFactors = FALSE
  ) %>% recode_error_type

  correct_responses <- error_types %>%
    filter(response == correct_response) %>%
    .$error_type
  expect_true(all(is.na(correct_responses)))

  word_trial_errors <- error_types %>%
    filter(response %in% c("left", "right"),
           correct_response %in% c("same", "different")) %>%
    .$error_type
  expect_true(all(word_trial_errors == "wrong_type"))
})

test_that("error type works for bilateral version", {
  error_types <- expand.grid(
    response_type = "word",
    response = c("left", "right"),
    correct_response = c("left", "right"),
    stringsAsFactors = FALSE
  ) %>% recode_error_type

  expect_equal(unique(error_types$error_type), c(NA, "wrong_key"))
})

test_that("error type works for unilateral version 2", {
  error_types <- data_frame(
    response_type = "word",
    keys_pressed = c("left left up", "up", "right up", "down", ""),
    response = c("up", "up", "up", "down", "timeout"),
    correct_response = "up"
  ) %>% recode_error_type

  wrong_key <- error_types %>%
    filter(response == "down", correct_response == "up") %>%
    .$error_type
  expect_equal(wrong_key, "wrong_key")

  wrong_types <- error_types %>%
    filter(keys_pressed %in% c("left left up", "right up")) %>%
    .$error_type
  expect_equal(wrong_types, c("wrong_type", "wrong_type"))
})
