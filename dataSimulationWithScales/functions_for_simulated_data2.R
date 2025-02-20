#### Functions to create likert values for an IV that is a median-split of a multi-item scale

#####################################################################
### Function to generate likert values for one IV
#
generate_likert_scales <- function(N, scaleLength, likertMin, likertMax, ivName = "A") {
  ## Note that N is the n per cell, and there are 4 cells because it is always a 2x2
  # Create a dataframe with N*4 rows
  df3 <- data.frame(matrix(nrow = N * 4, ncol = 0), stringsAsFactors = FALSE)
  
  # Generate scale items (a1 to a[scaleLength])
  for (i in 1:scaleLength) {
    df3[[paste0(ivName, "_", i)]] <- sample(likertMin:likertMax, size = N * 4, replace = TRUE)
  }
  
  # Compute the mean of all scale items
  df3[[paste0(ivName, "_Mean")]] <- rowMeans(df3[, paste0(ivName, "_", 1:scaleLength)])
  
  # Sort by A_Mean
  df3 <- df3[order(df3[[paste0(ivName, "_Mean")]], decreasing = TRUE), ]
  
  # Assign A values based on ranking
  df3[[ivName]] <- rep(c(paste0(ivName, "1"), paste0(ivName, "2")), each = N*2)
  
  # Identify duplicate A_Mean values between A1 and A2
  duplicate_means <- intersect(df3[[paste0(ivName, "_Mean")]][df3[[ivName]] == paste0(ivName, "1")], df3[[paste0(ivName, "_Mean")]][df3[[ivName]] == paste0(ivName, "2")])
  
  if (length(duplicate_means) > 0) {
    for (mean_value in duplicate_means) {
      affected_rows <- which(df3[[ivName]] == paste0(ivName, "1") & df3[[paste0(ivName, "_Mean")]] == mean_value)
      for (row in affected_rows) {
        # Find the column with the minimum integer value
        min_col <- which.min(df3[row, paste0(ivName, "_", 1:scaleLength)])
        # Increment that value by 1
        df3[row, paste0(ivName, "_", min_col)] <- df3[row, paste0(ivName, "_", min_col)] + 1
      }
    }
    # Recalculate the means
    df3[[paste0(ivName, "_Mean")]] <- rowMeans(df3[, paste0(ivName, "_", 1:scaleLength)])
  }
  
  return(df3)
}
#
# Example usage
## df_A <- generate_likert_scales(N = 5, scaleLength = 4, likertMin = 1, likertMax = 7, ivName="A")
## View(df_A)
#####################################################################



##################################################
# function to reorder the second (B) dataframe
reorder_dataframe <- function(df) {
  n <- nrow(df)
  if (n %% 4 != 0) {
    stop("Number of rows must be divisible by 4")
  }
  
  # Compute indices for each group
  group_size <- n / 4
  a_idx <- 1:group_size
  b_idx <- (group_size + 1):(2 * group_size)
  c_idx <- (2 * group_size + 1):(3 * group_size)
  d_idx <- (3 * group_size + 1):n
  
  # New order: a, c, b, d
  new_order <- c(a_idx, c_idx, b_idx, d_idx)
  
  return(df[new_order, , drop = FALSE])
}
##################################################


##################################################
# function to create some demographric characteristics
generate_demographic_df <- function(df, 
                                    gender_prop = 0.5, 
                                    age_min = 18, 
                                    age_max = 26, 
                                    raceProportion = 1, 
                                    race_1 = "race_1", 
                                    race_2 = "race_2", 
                                    race_3 = "race_3", 
                                    race_4 = "race_4") {
  # Assign Gender with proportions gender_prop, 1 - gender, 0.1
  df$Gender <- sample(c("M", "F", "O"), size = nrow(df), replace = TRUE, 
                      prob = c(gender_prop, 1 - gender_prop, 0.1))
  
  # Assign Age with random values between age_min and age_max
  df$Age <- sample(seq(age_min, age_max), size = nrow(df), replace = TRUE)
  
  # Assign Race with specified ratio
  race_values <- c(rep(race_1, raceProportion), race_2, race_3, race_4)
  df$Race <- sample(race_values, size = nrow(df), replace = TRUE)

  return(df)
}
##################################################



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





