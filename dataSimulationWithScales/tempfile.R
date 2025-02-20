# 
source(file = "functions_for_simulated_data2.R")

# simulated_data.csv
dfbig <- read.csv("/Users/dallbrit/Downloads/simulated_data.csv")
# randomly select 20 rows
df <- sample_n(dfbig, 20)

df2 <- create_dv_scale(df)
