rm(list=ls())
setwd("~/workspace/r/display_opt/")

library(nloptr)
#flight
#hotel

Total_budget <- 688000
CA_flight_target <- 35000
CA_hotel_target <- 27500
Season <- 0

num_iter <- 10000
WB <- 1
WH <- 1
WF <- 1

file_historical <- "./data_input_historical_cost.csv"
file_budseas <- "./data_input_budgetseason.csv"
file_to_pred <- "./data_input_topred.csv"
file_DR_dataset <- "./data_input_datarobot.csv"

source("./displayopt_lib_v1.R")
model <- runModelAll(file_historical, file_budseas)
# df_pred <- prepareTSDataPred(file_to_pred, Season, Total_budget)

fn1 <- function (b) {
  (b[1]+b[2]+b[3]+b[4]+b[5]+b[6]+b[7]+b[8]+b[9]+b[10]+b[11])
}

eq <- function(b) {
  # constr <- predictModelAll(b, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_pred)
  constr <- predictModelAll(b)
  c1 <- WF*(CA_flight_target - constr$CA_flight)
  c2 <- WH*(CA_hotel_target - constr$CA_hotel)
  c3 <- WB*((b[1]+b[2]+b[3]+b[4]+b[5]+b[6]+b[7]+b[8]+b[9]+b[10]+b[11]) - Total_budget)
  Sys.sleep(0.01)
  return(c(c1,c2,c3))
}

t0 <- proc.time()
res <- nloptr(x0=c(rep(0,11)),
              eval_f=fn1,
              lb = c(rep(0,11)),
              ub = c(16987.33,241097.7,74782.76,84995.68,45363.65,235207.6,
                     525.441,21842.27,3772.961,30000,53340.3),
              eval_g_ineq = eq,
              opts = list("algorithm"="NLOPT_LN_AUGLAG","maxeval"=num_iter,"local_opts"=list("algorithm"="NLOPT_LN_COBYLA")))
t1 <- proc.time(); print(t1-t0)
print(res)

#file_save <- ""
#write.csv(res$solution,file_save)
