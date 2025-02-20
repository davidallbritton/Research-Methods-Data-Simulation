# 
######################################
# function to create scales for the DV
create_dv_scale <- function(df, mean_col = "DV", likert_min = 1, likert_max = 7, n_scale_items = 2, dv_name = "DV_Scale") {
  # Rescale mean_col to the Likert scale range
  min_val <- min(df[[mean_col]], na.rm = TRUE)
  max_val <- max(df[[mean_col]], na.rm = TRUE)
  df[[paste0(dv_name, "_target_mean")]] <- likert_min + (df[[mean_col]] - min_val) / (max_val - min_val) * (likert_max - likert_min)

  generate_valid_values <- function(target_mean, n_scale_items, likert_min, likert_max) {
    # Calculate the target sum (rounded to the nearest integer)
    target_sum <- target_mean * n_scale_items
    
    # Initialize random Likert scale values
    scale_items <- sample(likert_min:likert_max, n_scale_items, replace = TRUE)
    
    # Adjust values iteratively until the sum is within 1 of the target
    while (abs(sum(scale_items) - target_sum) >= 1) {
      current_sum <- sum(scale_items)
      diff <- target_sum - current_sum
      
      if (diff > 0) {
        # Increase the smallest value if possible
        idx <- which.min(scale_items)
        if (scale_items[idx] < likert_max) {
          scale_items[idx] <- scale_items[idx] + 1
        }
      } else {
        # Decrease the largest value if possible
        idx <- which.max(scale_items)
        if (scale_items[idx] > likert_min) {
          scale_items[idx] <- scale_items[idx] - 1
        }
      }
    }
    return(scale_items)
  }
  
  # Generate Likert-scale values
  for (i in seq_len(n_scale_items)) {
    df[[paste0(dv_name, "_", i)]] <- NA
  }

  for (row in seq_len(nrow(df))) {
    target_row_mean <- df[[paste0(dv_name, "_target_mean")]][row]
    scale_values <- generate_valid_values(target_row_mean, n_scale_items, likert_min, likert_max)
    scale_sum <- 0
    for (i in seq_len(n_scale_items)) {
      df[row, paste0(dv_name, "_", i)] <- scale_values[i]
      scale_sum <- scale_sum + scale_values[i]
    }
    df[row, paste0(dv_name, "_", "mean")] <- scale_sum / n_scale_items
  }

  return(df)
}
#
######################################
