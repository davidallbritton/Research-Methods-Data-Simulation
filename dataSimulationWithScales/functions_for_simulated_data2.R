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



