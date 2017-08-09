rm(list=ls())
# setwd("~/workspace/r/display_opt/")
setwd("~/MTA_Display_Opt/")
source("./nloptr.R")

file_budget_season <- "./data_input_budgetseason.csv"
file_DR_dataset <- "./data_input_datarobot.csv"

# Iteration-1
file_historical <- "./data_input_historical_cost.csv"
file_to_pred <- "./data_input_topred.csv"
file_out <- "./data_output_result_2017_07_v2.csv"
Total_budget <- 688000
Season <- 0
CA_flight_target <- 34500
CA_hotel_target <- 25500
df_result <-   runNLOptR(file_historical, file_budget_season, file_to_pred, file_DR_dataset, Total_budget, Season, CA_flight_target, CA_hotel_target, forceMaxBudget=TRUE)
write.csv(df_result,file_out, row.names = FALSE)
# Iteration-2
df_result <- read.csv(file_out, header=TRUE); write.csv(df_result, "./df_result_short.csv", row.names=FALSE)
CA_flight_prev <- df_result[1,"Total_CA_flight"]
CA_hotel_prev <- df_result[1,"Total_CA_hotel"]
if ((CA_flight_prev < CA_flight_target) | (CA_hotel_prev < CA_hotel_target)) {
  df_result <- runNLOptR(file_historical, file_budget_season, file_to_pred, file_DR_dataset, Total_budget, Season, CA_flight_target, CA_hotel_target, forceMaxBudget=TRUE, init_budget=df_result$budget_channel, num_iter=1000)
  write.csv(df_result,file_out, row.names = FALSE)
}
