#' Recode all variables for this experiment.
#'
#' @param frame data.frame to recode.
#' @importFrom dplyr `%>%`
#' @export
recode <- function(frame) {
  frame %>%
    recode_cue_type %>%
    recode_mask_type %>%
    recode_response_type
}

recode_cue_type <- function(frame) {
  cue_type_map <- dplyr::data_frame(
    cue_type = c("invalid", "valid"),
    cue_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, cue_type_map))
  frame
}

recode_mask_type <- function(frame) {
  mask_type_map <- dplyr::data_frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, mask_type_map))
  frame
}

recode_response_type <- function(frame) {
  response_type_map <- dplyr::data_frame(
    response_type = c("pic", "word"),
    response_label = c("Upright picture", "Verify word"),
    response_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, response_type_map))
  frame
}

add_sig_stars <- function(frame) {
  frame %>% mutate(
    sig = ifelse(p.value > 0.05, "",
                 ifelse(p.value > 0.01, "*",
                        ifelse(p.value > 0.001, "**",
                               "***"))))
}
