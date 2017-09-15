rm(list=ls())
# setwd("~/workspace/r/display_opt/")
setwd("~/MTA_Display_Opt/")

file_dir <- "./data"
file_input_budget_season <- "data_input_budget_season.csv"
file_input_DR_dataset <- "data_input_datarobot.csv"
file_input_historical_cost <- "data_input_historical_cost.csv"
file_input_to_pred <- "data_input_to_pred.csv"
file_out            <- "./data/data_output_result_2017_07_v3.csv"
Total_budget <- 688000
Season <- 0
CA_flight_target <- 34500
CA_hotel_target <- 25000

# Initialize
file_budget_season  <- paste(file_dir, "/", file_input_budget_season, sep='')
file_DR_dataset     <- paste(file_dir, "/", file_input_DR_dataset, sep='')
file_historical     <- paste(file_dir, "/", file_input_historical_cost, sep='')
file_to_pred        <- paste(file_dir, "/", file_input_to_pred, sep='')
source("./prepare_data.R")
source("./nloptr.R")

# Iteration-1
val <-   runNLOptR(file_historical, file_budget_season, file_to_pred, file_DR_dataset, Total_budget, Season, CA_flight_target, CA_hotel_target, num_iter=1000, retrain=FALSE)
model <- val$model
df_result <- val$df
write.csv(df_result,file_out, row.names = FALSE)

# Iteration-2
df_result <- read.csv(file_out, header=TRUE); write.csv(df_result, "./data/tmp_df_result_short.csv", row.names=FALSE)
CA_flight_prev <- df_result[1,"Total_CA_flight"]
CA_hotel_prev <- df_result[1,"Total_CA_hotel"]
if ((CA_flight_prev < CA_flight_target) | (CA_hotel_prev < CA_hotel_target)) {
  val <- runNLOptR(file_historical, file_budget_season, file_to_pred, file_DR_dataset, Total_budget, Season, CA_flight_target, CA_hotel_target, num_iter=10000, init_budget=df_result$budget_channel)
  df_result <- val$df
  write.csv(df_result,file_out, row.names = FALSE)
}

## Manual tweak
if (FALSE) {
  df_result <- read.csv(file_out, header=TRUE)
  nlopt_param <- getNLOptParam(Total_budget, file_DR_dataset, df_result)
  ch_lo <- c("android - BBM App Install","android - UAC App Install","ios - inMobi App Install","ios - Mundomedia App Install","ios - FB App Install")
  ch_hi <- c("ios - UAC App Install","android - Iron Source App Install","ios - BBM App Install")
  # 18.2k, 18.3k, 18.3k
  gap <- 0
  for (ch in ch_lo){
    cch <- strsplit(ch," - ")[[1]]; ch_pl <- cch[1]; ch_cg <- cch[2]
    idx <- (df_result$platform==ch_pl & df_result$channel_group==ch_cg)
    gap <- gap + df_result[idx,"budget_channel"] - nlopt_param$lb[idx]
    df_result[idx,"budget_channel"] <- nlopt_param$lb[idx]
  }
  #prop <- rep(1/length(ch_hi), length(ch_hi))
  prop <- c(0.3333,0.3333,0.3333)
  for (i in 1:length(ch_hi)){
    ch <- ch_hi[i]
    cch <- strsplit(ch," - ")[[1]]; ch_pl <- cch[1]; ch_cg <- cch[2]
    idx <- (df_result$platform==ch_pl & df_result$channel_group==ch_cg)
    df_result[idx,"budget_channel"] <- df_result[idx,"budget_channel"] + (gap*prop[i])
  }
  # df_result <- normalizeResult(Total_budget, file_DR_dataset, df_result)
  constr <- predictModelAll(df_result$budget_channel, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_result)
  df_result$CA_flight <- constr$tbl$CA_flight
  df_result$CA_hotel  <- constr$tbl$CA_hotel
  df_result$Total_CA_flight <- constr$CA_flight
  df_result$Total_CA_hotel  <- constr$CA_hotel
}

# Check Upper Bound Budget vs CA
df_result_max <- df_result
df_result_max$budget_channel <- getNLOptParam(Total_budget, file_DR_dataset, df_result)$ub
df_result_max$budget <- sum(df_result_max$budget_channel)
constr <- predictModelAll(df_result_max$budget_channel, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_result_max)
df_result_max$CA_flight <- constr$tbl$CA_flight
df_result_max$CA_hotel  <- constr$tbl$CA_hotel
df_result_max$Total_CA_flight <- constr$CA_flight
df_result_max$Total_CA_hotel  <- constr$CA_hotel
