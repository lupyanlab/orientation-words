clean <- function(frame) {
  frame %>%
    filter(
      # Remove practice trials
      block_type != "practice",
      
      # Drop weird RTs
      rt < 2000
    ) %>%
    mutate(
      # Drop RT on incorrect response trials
      rt = ifelse(is_correct, rt, NA),
      
      # Drop accuracy on timeout trials
      is_correct = ifelse(response == "timeout", NA, is_correct),

      # Create is_error from is_correct
      is_error = ifelse(is_correct, 0, 1)
    )
}