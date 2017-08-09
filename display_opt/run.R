rm(list=ls())
setwd("~/workspace/r/display_opt/")
# setwd("~/MTA_Display_Opt/")
source("./nloptr.R")

file_budget_season <- "./data_input_budgetseason.csv"
file_DR_dataset <- "./data_input_datarobot.csv"

file_historical <- "./data_input_historical_cost.csv"
file_to_pred <- "./data_input_topred.csv"
file_out <- "./data_output_result_2017_07.csv"
Total_budget <- 688000
Season <- 0
CA_flight_target <- 35000
CA_hotel_target <- 22000
df_result <- runNLOptR(file_historical, file_budget_season, file_to_pred, file_DR_dataset, Total_budget, Season, CA_flight_target, CA_hotel_target)
write.csv(df_result,file_out)
