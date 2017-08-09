rm(list=ls())
setwd("~/workspace/r/display_opt/")
# setwd("~/MTA_Display_Opt/")

Total_budget <- 688000
CA_flight_target <- 35000
CA_hotel_target <- 27500
Season <- 0

file_historical <- "./data_input_historical_cost.csv"
file_budseas <- "./data_input_budgetseason.csv"
file_to_pred <- "./data_input_topred.csv"
file_DR_dataset <- "./data_input_datarobot.csv"

source("./displayopt_lib.R")

source("./displayopt_lib.R")
model <- runModelAll(file_historical, file_budseas)
# model <- runModelAll(file_historical, file_budseas, file_DR_dataset) # Only if u want to retrain the Datarobot model
df_pred <- prepareTSDataPred(file_to_pred, Season, Total_budget)

# b <- c(16987.33,241097.7,74782.76,84995.68,45363.65,235207.6, 525.441,21842.27,3772.961,30000,53340.3)
b <- c(21600, 105600, 65600, 75600, 45600, 241000, 6100, 25600, 9100, 35600, 55600)
sum(b)
res <- predictModelAll(b, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_pred)
print(res)

