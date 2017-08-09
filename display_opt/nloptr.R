library(nloptr)

runNLOptR <- function(file_historical, file_budget_season, file_to_pred, file_DR_dataset, Total_budget, Season, CA_flight_target, CA_hotel_target, num_iter=NULL, retrain=FALSE) {
  nlopt_num_iter <- num_iter
  nlopt_xtol_abs <- 100
  WB <- 1
  WH <- 1
  WF <- 1
  
  source("./displayopt_lib.R")
  if (retrain) {
    model <- runModelAll(file_historical, file_budget_season, file_DR_dataset)
  } else {
    model <- runModelAll(file_historical, file_budget_season)
  }
  df_pred <- prepareTSDataPred(file_to_pred, Season, Total_budget)
  
  fn1 <- function (b) {
    (b[1]+b[2]+b[3]+b[4]+b[5]+b[6]+b[7]+b[8]+b[9]+b[10]+b[11])
  }
  
  eq <- function(b) {
    constr <- predictModelAll(b, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_pred)
    c1 <- WF*(CA_flight_target - constr$CA_flight)
    c2 <- WH*(CA_hotel_target - constr$CA_hotel)
    c3 <- WB*((b[1]+b[2]+b[3]+b[4]+b[5]+b[6]+b[7]+b[8]+b[9]+b[10]+b[11]) - Total_budget)
    # Sys.sleep(0.005)
    return(c(c1,c2,c3))
  }
  
  B_lower <- getNLOptParam(Total_budget, file_DR_dataset, df_pred)$lb
  B_upper <- getNLOptParam(Total_budget, file_DR_dataset, df_pred)$ub
  init_point <- getNLOptParam(Total_budget, file_DR_dataset, df_pred)$init
  # print(paste("LB =",B_lower))
  # print(paste("Init =",init_point))
  # print(paste("UB =",B_upper))
  if (is.null(num_iter)){
    nlopt_con <- "xtol_abs"
    nlopt_val <- nlopt_xtol_abs
  } else {
    nlopt_con <- "maxeval"
    nlopt_val <- nlopt_num_iter
  }
  t0 <- proc.time()
  res <- nloptr(
    x0 = init_point,
    eval_f=fn1,
    lb = B_lower,
    ub = B_upper,
    eval_g_ineq = eq,
    opts = list("algorithm"="NLOPT_LN_AUGLAG",nlopt_con=nlopt_val,"local_opts"=list("algorithm"="NLOPT_LN_COBYLA")))
  t1 <- proc.time(); print(t1-t0) # 1800 sec
  
  df_result <- df_pred
  df_result$budget_channel <- res$solution
  constr <- predictModelAll(df_result$budget_channel, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_result)
  df_result$budget <- sum(df_result$budget_channel)
  df_result$CA_flight <- constr$CA_flight
  df_result$CA_hotel <- constr$CA_hotel
  # View(df_result)
  # file_save <- "./data_output_result.csv"
  # write.csv(df_result,file_save)
  
  ## Additional step
  # df_result <- read.csv("./data_output_result.csv", header=TRUE)
  if (sum(df_result$budget_channel) < Total_budget){
    df_result$budget_channel <- df_result$budget_channel * Total_budget / df_result$budget
    df_result$budget <- Total_budget
    constr <- predictModelAll(df_result$budget_channel, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_result)
    df_result$CA_flight <- constr$CA_flight
    df_result$CA_hotel <- constr$CA_hotel
    # View(df_result)
    # file_save <- "./data_output_result_fixed.csv"
    # write.csv(df_result,file_save)
  }
  return(df_result)
}

