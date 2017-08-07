rm(list=ls())

Total_budget <- 688000
CA_flight_target <- 35000
CA_hotel_target <- 27500
Season <- 0

file_historical <- "./data_input_historical_cost.csv"
file_budseas <- "./data_input_budgetseason.csv"
file_topred <- "./data_input_topred_budgetseason.csv"
file_DR_dataset <- "./data_input_datarobot.csv"

## (1) Timeseries predicting Cost
# a. Prepare dataset
df_historical <- read.csv(file_historical,header=TRUE)
df_budseas <- read.csv(file_budseas, header = TRUE)
# b. Create timeseries model using BSTS
source("./myscript_v2_pkg.R")
model_ts <- createModel(df_historical, df_budseas)

## (2) Fitting CA using Datarobot API
source("./datarobot_template.R")
# a. Prepare dataset & run model training
# df_dr_raw <- read.csv(file_DR_dataset)
# df <- prepareDRDataset(df_dr_raw)
# proj_CA_flight <- trainDRModel(df, "CA_flight")
# proj_CA_hotel <- trainDRModel(df, "CA_hotel")
# b. Get the project & model
proj_CA_flight <- getDRProject("CA_flight")
proj_CA_hotel <- getDRProject("CA_hotel")
model_CA_flight <- getDRModelBest(proj_CA_flight)
model_CA_hotel <- getDRModelBest(proj_CA_hotel)

## (3) Predict the CA given budget
# a. Predict the future cost for each channel
df_pred <- read.csv(file_topred, header = TRUE)
df_pred$season <- Season
df_pred$budget <- Total_budget / 1e5
budget_prop <- c(16987.33,241097.7,74782.76,84995.68,45363.65,235207.6, 525.441,21842.27,3772.961,30000,53340.3)
data_pred <- predictModel(df_pred, budget_prop, model_ts)
# b. Predict the future CA for each channel
df_pred_CA <- prepareDRDataPred(data_pred, Total_budget, CA_flight_target, CA_hotel_target)
df_pred_CA <- predictDRModel(df_pred_CA, proj_CA_flight, model_CA_flight, "CA_flight")
df_pred_CA <- predictDRModel(df_pred_CA, proj_CA_hotel, model_CA_hotel, "CA_hotel")
CA_flight <- sum(df_pred_CA$CA_flight)
CA_hotel <- sum(df_pred_CA$CA_hotel)

