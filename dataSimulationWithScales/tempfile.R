# 
source(file = "functions_for_simulated_data2.R")

# Example usage
n = 2500
dfa <- generate_likert_scales(N = n, scaleLength = 4, likertMin = 1, likertMax = 7, ivName = "A")
dfb <- generate_likert_scales(N = n, scaleLength = 4, likertMin = 1, likertMax = 7, ivName = "B")
dfc <- reorder_dataframe(dfb)
dfd <- cbind(dfa, dfc)


dfx <- generate_demographic_df(dfd, 
                                gender_prop = 0.5, 
                                age_min = 18, 
                                age_max = 26, 
                                raceProportion = 4, 
                                race_1 = "race_1", 
                                race_2 = "race_2", 
                                race_3 = "race_3", 
                                race_4 = "race_4") 

dfy <- generate_demographic_df(dfd)  

table(dfx$Gender)  
table(dfx$Race)
table(dfx$Age)

table(dfy$Gender)  
table(dfy$Race)
table(dfy$Age)


dfx <- generate_demographic_df(dfd, 
                               gender_prop = gender_prop, 
                               age_min = age_min, 
                               age_max = age_max, 
                               raceProportion = raceProportion, 
                               race_1 = race_1, 
                               race_2 = race_2, 
                               race_3 = race_3, 
                               race_4 = race_4) 

dfx <- generate_demographic_df(dfd, 
                               gender_prop = input$gender / 100, 
                               age_min = input$age_min, 
                               age_max = input$age_max, 
                               raceProportion = input$raceProportion, 
                               race_1 = input$race_1, 
                               race_2 = input$race_2, 
                               race_3 = input$race_3, 
                               race_4 = input$race_4) 





