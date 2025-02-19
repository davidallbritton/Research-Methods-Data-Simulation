generate_likert_data <- function(N, scaleLength, likertMin, likertMax, ivName = "A") {
  # Create a dataframe with N*2 rows
  df3 <- data.frame(matrix(nrow = N * 2, ncol = 0), stringsAsFactors = FALSE)
  
  # Generate scale items (a1 to a[scaleLength])
  for (i in 1:scaleLength) {
    df3[[paste0(ivName, "_", i)]] <- sample(likertMin:likertMax, size = N * 2, replace = TRUE)
  }
  
  # Compute the mean of all scale items
  df3[[paste0(ivName, "_Mean")]] <- rowMeans(df3[, paste0(ivName, "_", 1:scaleLength)])
  
  # Sort by A_Mean
  df3 <- df3[order(df3[[paste0(ivName, "_Mean")]], decreasing = TRUE), ]
  
  # Assign A values based on ranking
  df3[[ivName]] <- rep(c(paste0(ivName, "1"), paste0(ivName, "2")), each = N)
  
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

# Example usage
df3 <- generate_likert_data(N = 5, scaleLength = 4, likertMin = 1, likertMax = 7)
View(df3)
