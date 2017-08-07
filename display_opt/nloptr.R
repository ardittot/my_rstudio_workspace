rm(list=ls())
setwd("~/workspace/r/display_opt/")

library(nloptr)
#flight
#hotel

Total_budget <- 688000
Season <- 0
CA_flight_target <- 35000
CA_hotel_target <- 27500

num_iter <- 100
WB <- 1
WH <- 1
WF <- 1

file_historical <- "./data_input_historical_cost.csv"
file_budseas <- "./data_input_budgetseason.csv"
file_to_pred <- "./data_input_topred.csv"
file_DR_dataset <- "./data_input_datarobot.csv"

source("./displayopt_lib.R")
model <- runModelAll(file_historical, file_budseas)
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

B_lower <- rep(0,11)
B_upper <- c(16987.33,241097.7,74782.76,84995.68,45363.65,235207.6,525.441,21842.27,3772.961,30000,53340.3)
init_point <- B_upper - 100
t0 <- proc.time()
res <- nloptr(
    x0 = init_point,
    eval_f=fn1,
    lb = B_lower,
    ub = B_upper,
    eval_g_ineq = eq,
    opts = list("algorithm"="NLOPT_LN_AUGLAG","maxeval"=num_iter,"local_opts"=list("algorithm"="NLOPT_LN_COBYLA")))
t1 <- proc.time(); print(t1-t0) # 1822 sec
print(res$solution)

df_pred$budget_channel <- res$solution
sum(df_pred$budget_channel) # = 767873.5
constr <- predictModelAll(df_pred$budget_channel, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_pred)
constr$CA_flight # = 29657.7
constr$CA_hotel # = 11595.33

#file_save <- ""
#write.csv(res$solution,file_save)