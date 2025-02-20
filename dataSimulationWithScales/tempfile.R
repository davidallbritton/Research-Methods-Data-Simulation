# 
source(file = "functions_for_simulated_data2.R")

# Example usage
dfa <- generate_likert_scales(N = 2, scaleLength = 4, likertMin = 1, likertMax = 7, ivName = "A")
dfb <- generate_likert_scales(N = 2, scaleLength = 4, likertMin = 1, likertMax = 7, ivName = "B")
dfc <- reorder_dataframe(dfb)

dfd <- cbind(dfa, dfd)


