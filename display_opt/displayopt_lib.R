## *** Model 1: Timeseries model to predict Cost *** ##
# install.packages(c("tseries","forecast","xts","gridExtra","lubridate","bsts"))
library(forecast)
library(tseries)
# library(TTR)
library(ggplot2)
library(xts)
require(gridExtra)
# require(lmtest)
library(lubridate)
# require(astsa)
library(bsts)
library(reshape)
library(dplyr)

createModel <- function(df_historical,df_budseas){
  #### Seasonality & Budget ####
  # df_budseas <- read.csv(file_budseas, header = TRUE)
  df_budseas <- df_budseas %>%
    mutate(newmonth=as.Date(month,"%m/%d/%Y")) %>%
    mutate(month=format(newmonth, "%Y/%m/%d")) %>%
    select(-newmonth) %>%
    mutate(channel=(paste(platform,"-",channel_group))) %>%
    mutate(channel=factor(channel)) %>%
    mutate(month=as.Date(month)) %>%
    select(month,channel,season,budget)
  
  #### Historical Data ####
  # df_historical <- read.csv(file_historical,header=TRUE)
  df <- df_historical %>%
    mutate(channel=paste(platform,"-",channel_group)) %>%
    select(reporting_date,channel,total_cost_usd) %>%
    dplyr::rename(cost=total_cost_usd, date=reporting_date)
  
  df <- df %>%
    mutate(newdate=as.Date(date,"%m/%d/%Y")) %>%
    mutate(date=format(newdate, "%Y/%m/%d")) %>%
    mutate(month=format(newdate, "%Y/%m/01")) %>%
    dplyr::select(-newdate) %>%
    mutate(channel=ifelse((channel=="android - Facebook App Install" | channel=="android - Facebook DPA" | channel=="android - Instagram App Install"), "android - FB App Install", channel)) %>%
    mutate(channel=ifelse((channel=="ios - Facebook App Install" | channel=="ios - Facebook DPA" | channel=="ios - Instagram App Install"), "ios - FB App Install", channel)) %>%
    mutate(channel=ifelse((channel=="android - UAC App Install" | channel=="android - GDN App Install"), "android - UAC App Install", channel)) %>%
    mutate(channel=ifelse((channel=="ios - UAC App Install" | channel=="ios - GDN App Install"), "ios - UAC App Install", channel)) %>%
    mutate(channel=factor(channel)) %>%
    dplyr::select(date,month,channel,cost)
  
  # tmp <- reshape2::dcast(cbind(df, cnt=rep(1,nrow(df))), value.var = "cnt", date ~ channel, sum)
  df <- df %>%
    group_by(date,month,channel) %>%
    summarise(cost = max(cost)) %>%
    group_by(month,channel) %>%
    summarise(cost = sum(cost)) %>%
    data.frame() %>%
    mutate(month=as.Date(month, "%Y/%m/%d")) %>%
    group_by(month) %>%
    mutate(cost_sum=sum(cost)) %>%
    mutate(channel_prop=(cost/cost_sum)) %>%
    select(-cost_sum) %>%
    as.data.frame() %>%
    filter(cost>0)
  
  ## Get monthly budget and seasonality factor
  df <- df %>%
    left_join(df_budseas, by = c("month","channel")) %>%
    mutate(channel_prop=channel_prop*budget) %>%
    rename(budget_prop=channel_prop) %>%
    mutate(channel=factor(channel)) %>%
    mutate(season=as.numeric(season)) %>%
    select(month,channel,season,budget,budget_prop,cost)
  
  ## Check completeness of each channel data
  check_completeness <- reshape2::dcast(cbind(df, cnt=rep(1,nrow(df))), value.var = "cnt", month ~ channel, sum)
  
  # data_train <- df[df$month<max(df$month),]
  # data_test <- df[df$month>=max(df$month),]
  data_train <- df
  
  y <- xts(data_train[,"cost"],order.by=data_train[,"month"])
  colnames(y) <- c("cost")
  # Seasonal component of num_trx
  seas <- ts(na.omit(data_train$cost), frequency=12)
  decomp <- stl(seas, s.window="periodic")
  y_ss <- decomp$time.series[,"trend"] + decomp$time.series[,"seasonal"]
  data_train <- data_train %>% select(-cost)
  
  ss <- list()
  ss <- AddLocalLinearTrend(ss, y_ss)
  # ss <- AddSeasonal(ss, y_ss, nseasons = 12)
  model <- bsts(y ~ (. - month - 1), state.specification = ss, niter = 4000, seed=99, ping = 4000/10, data = data_train)
  return(model)
}

predictModel <- function(df_topred, budget_prop, model){
  
  data_test <- df_topred
  data_test <- data_test %>%
    mutate(channel=paste(platform,"-",channel_group)) %>%
    mutate(month = as.Date(month, "%m/%d/%Y")) %>%
    mutate(channel=factor(channel)) %>%
    mutate(season=as.numeric(season)) %>%
    select(month,channel,season,budget)
  data_test$budget_prop <- budget_prop/(1e5)
  
  cost_pred <- c()
  for (i in 1:nrow(data_test)){
    # toPred <- filter(data_test, channel==ch)
    toPred <- data_test[i,]
    pred <- predict(model, burn = 100, quantiles = c(0.25, 0.75), newdata = toPred)
    res <- pred$median
    cost_pred <- c(cost_pred, res)
  }
  data_test$cost <- cost_pred
  return(data_test)
}

prepareTSDataPred <- function(file_to_pred, Season, Total_budget){
  df_pred <- read.csv(file_to_pred, header = TRUE)
  df_pred$season <- Season
  df_pred$budget <- Total_budget / 1e5
  return(df_pred)
}

## *** Model 2: Regression model to predict CA *** ##
library(datarobot)
ConnectToDataRobot(token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N', endpoint ='http://data-datarobot-apps-01.prod.data.tvlk.cloud/api/v2')
#ConnectToDataRobot(token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N', endpoint ='http://tvlk-data-datarobot-apps-lb-1080587125.ap-southeast-1.elb.amazonaws.com/api/v2')

prepareDRDataset <- function(df){
  # df$Channel <- fixChannelName(df$Channel) # spl <- strsplit(x, "-")[[1]]
  res <- df %>% 
    mutate(month=as.Date(month, "%m/%d/%Y")) %>%
    mutate(tier=factor(tier, levels = c("NORMAL","HIGH"))) %>%
    # mutate(Channel=fixChannelName(Channel)) %>%
    mutate(budget=as.numeric(budget)) %>%
    mutate(CA_flight_target=as.numeric(CA_flight_target)) %>%
    mutate(CA_hotel_target=as.numeric(CA_hotel_target)) %>%
    mutate(cost=as.numeric(cost)) %>%
    mutate(Sum_Install=as.numeric(Sum_Install)) %>%
    mutate(CA_flight=as.numeric(CA_flight)) %>%
    mutate(CA_hotel=as.numeric(CA_hotel))
  res_HIGH <- res %>% filter(tier=="HIGH")
  # res_HIGH <- res_HIGH[sample(1:nrow(res_HIGH), nrow(res_HIGH), replace=TRUE),]
  res <- rbind(res, res_HIGH) %>% arrange(month,channel)
  return(res)
}

trainDRModel <- function(df, response_var){
  lcol <- c("channel","tier","budget","cost",paste(response_var,"_target",sep = ""),response_var)
  df1 <- df[,lcol]
  df1 <- df1 %>% filter(!is.na(cost))
  project_name <- paste("MTA: Display Opt -",response_var)
  project <- SetupProject(dataSource = df1, projectName = project_name)
  SetTarget(project = project, target = response_var, metric = "RMSE")
  WaitForAutopilot(project = project)
  return(project)
}

getDRProject <- function(proj_name){
  proj_id <- GetProjectList()$projectId[GetProjectList()$projectName %in% proj_name]
  proj_id <- proj_id[1]
  proj <- GetProject(proj_id)
  return(proj)
}

getDRProjectMultiple <- function(proj_name){
  proj_id <- GetProjectList()$projectId[GetProjectList()$projectName %in% proj_name]
  return(proj_id)
}

getDRModelBest <- function(project){
  listOfModels <- GetAllModels(project)
  # summary(listOfModels)
  # plot(listOfModels, orderDecreasing = TRUE)
  
  modelFrame <- as.data.frame(listOfModels)
  # modelType <- modelFrame$modelType
  metric <- modelFrame$validationMetric
  # bestModelType <- modelType[which.min(metric)]
  # worstModelType <- modelType[which.max(metric)]
  # modelFrame$expandedModel
  bestIndex <- which.min(metric)
  bestModel <- listOfModels[[bestIndex]]
  return(bestModel)
}

prepareDRDataPred <- function(df, Total_budget, CA_flight_target, CA_hotel_target){
  df_pred_dr <- df %>%
    select(channel,cost) %>%
    mutate(tier=ifelse(channel %in% c("android - FB App Install","android - inMobi App Install","android - UAC App Install","android - UAC App Install"),"HIGH","NORMAL")) %>%
    mutate(budget=Total_budget) %>%
    mutate(CA_flight_target=CA_flight_target) %>%
    mutate(CA_hotel_target=CA_hotel_target) %>%
    mutate(CA_flight=NaN) %>%
    mutate(CA_hotel=NaN)
  return(df_pred_dr)
}

predictDRModel <- function(df, project, model, response_var){
  res <- df
  dataset <- UploadPredictionDataset(project, res)
  bestPredictJobId <- RequestPredictionsForDataset(project, model$modelId, dataset$id)
  bestPredictions <- GetPredictions(project, bestPredictJobId)
  res[,response_var] <- bestPredictions
  DeletePredictionDataset(project, dataset$id)
  return(res)
}

## *** Model 3: Predicting CA given budget *** ##
runModelAll <- function(file_historical, file_budget_season, file_DR_dataset=NULL){
  ## (1) Timeseries predicting Cost
  # a. Prepare dataset
  df_historical <- read.csv(file_historical,header=TRUE)
  df_budseas <- read.csv(file_budget_season, header = TRUE)
  # b. Create timeseries model using BSTS
  # source("./myscript_v2_pkg.R")
  model_ts <- createModel(df_historical, df_budseas)
  
  ## (2) Fitting CA using Datarobot API
  # source("./datarobot_template.R")
  if (!is.null(file_DR_dataset)){
    # a. Prepare dataset & run model training
    df_dr_raw <- read.csv(file_DR_dataset)
    df <- prepareDRDataset(df_dr_raw)
    # Rename the existing project into the old one
    proj_CA_flight <- getDRProject("MTA: Display Opt - CA_flight")
    proj_CA_hotel  <- getDRProject("MTA: Display Opt - CA_hotel")
    UpdateProject(project=proj_CA_flight, newProjectName="MTA: Display Opt - CA_flight (old)")
    UpdateProject(project=proj_CA_hotel, newProjectName="MTA: Display Opt - CA_hotel (old)")
    # Remove the older projects
    proj_CA_flight <- getDRProjectMultiple("MTA: Display Opt - CA_flight (old)")
    proj_CA_hotel  <- getDRProjectMultiple("MTA: Display Opt - CA_hotel (old)")
    if (length(proj_CA_flight)>1) { for (p in proj_CA_flight[2:length(proj_CA_flight)]){ DeleteProject(p) } }
    if (length(proj_CA_hotel)>1) { for (p in proj_CA_hotel[2:length(proj_CA_flight)]){ DeleteProject(p) } }
    # Train the model
    proj_CA_flight <- trainDRModel(df, "CA_flight")
    proj_CA_hotel <- trainDRModel(df, "CA_hotel")
  } else {
    # b. Get existing project & model
    proj_CA_flight <- getDRProject("MTA: Display Opt - CA_flight")
    proj_CA_hotel  <- getDRProject("MTA: Display Opt - CA_hotel")
  }
  model_CA_flight <- getDRModelBest(proj_CA_flight)
  model_CA_hotel <- getDRModelBest(proj_CA_hotel)
  model <- list(
    "model_ts"=model_ts,
    "proj_CA_flight"=proj_CA_flight,
    "proj_CA_hotel"=proj_CA_hotel,
    "model_CA_flight"=model_CA_flight,
    "model_CA_hotel"=model_CA_hotel
  )
  return(model)
}

prepareDRDataPred <- function(df, Total_budget, CA_flight_target, CA_hotel_target){
  df_pred_dr <- df %>%
    select(channel,cost) %>%
    mutate(tier=ifelse(channel %in% c("android - FB App Install","android - inMobi App Install","android - UAC App Install","android - UAC App Install"),"HIGH","NORMAL")) %>%
    mutate(budget=Total_budget) %>%
    mutate(CA_flight_target=CA_flight_target) %>%
    mutate(CA_hotel_target=CA_hotel_target) %>%
    mutate(CA_flight=NaN) %>%
    mutate(CA_hotel=NaN)
  return(df_pred_dr)
}

predictModelAll <- function(budget_prop, Season, Total_budget, CA_flight_target, CA_hotel_target, model, df_pred){
  PARAM_Season <- Season
  PARAM_Total_budget <- Total_budget
  PARAM_CA_flight_target <- CA_flight_target
  PARAM_CA_hotel_target <- CA_hotel_target
  PARAM_model_ts <- model$model_ts
  PARAM_model_CA_flight <- model$model_CA_flight
  PARAM_model_CA_hotel <- model$model_CA_hotel
  PARAM_proj_CA_flight <- model$proj_CA_flight
  PARAM_proj_CA_hotel <- model$proj_CA_hotel
  PARAM_df_pred <- df_pred
  # budget_prop <- c(16987.33,241097.7,74782.76,84995.68,45363.65,235207.6, 525.441,21842.27,3772.961,30000,53340.3)
  ## (3) Predict the CA given budget
  # a. Predict the future cost for each channel
  data_pred <- predictModel(df_pred, budget_prop, PARAM_model_ts)
  # b. Predict the future CA for each channel
  df_pred_CA <- prepareDRDataPred(data_pred, PARAM_Total_budget, PARAM_CA_flight_target, PARAM_CA_hotel_target)
  df_pred_CA <- predictDRModel(df_pred_CA, PARAM_model_CA_flight, PARAM_model_CA_flight, "CA_flight")
  df_pred_CA <- predictDRModel(df_pred_CA, PARAM_model_CA_hotel, PARAM_model_CA_hotel, "CA_hotel")
  CA_flight <- sum(df_pred_CA$CA_flight)
  CA_hotel <- sum(df_pred_CA$CA_hotel)
  res <- list("CA_flight"=CA_flight, "CA_hotel"=CA_hotel, "tbl"=df_pred_CA)
  return(res)
}

## *** Model 4: Additional Functions *** ##
getNLOptParam <- function(Total_budget, file_DR_dataset, df_pred){
  df <- read.csv(file_DR_dataset)
  ub <- rep(Total_budget, nrow(df_pred))
  lb <- rep(0,nrow(df_pred))
  init <- rep(0, nrow(df_pred))
  for (i in 1:nrow(df_pred)){
    ch <- paste(df_pred$platform[i],"-",df_pred$channel_group[i])
    # ch <- "android - FB App Install"
    ch_cost <- df%>%
      mutate(month=as.Date(month, "%m/%d/%Y")) %>%
      filter((as.character(channel) == ch) & !(is.na(cost))) %>%
      mutate(cost=cost*Total_budget/budget) %>%
      arrange(month) %>%
      tail(3)
    maxcost <- min(Total_budget, quantile(ch_cost$cost, 0.99)*1.2)
    mincost <- max(0, quantile(ch_cost$cost, 0.00)*0.8)
    initcost <- min(Total_budget, quantile(ch_cost$cost,0.5))
    ub[i] <- maxcost; lb[i] <- mincost; init[i] <- initcost
  }
  res <- list("ub"=ub, "lb"=lb, "init"=init)
  return(res)
}

normalizeResult <- function(Total_budget, file_DR_dataset, df){
  df_data <- read.csv(file_DR_dataset)
  df_pred <- df
  ub <- rep(Total_budget, nrow(df_pred))
  lb <- rep(0,nrow(df_pred))
  init <- rep(0, nrow(df_pred))
  for (i in 1:nrow(df_pred)){
    ch <- paste(df_pred$platform[i],"-",df_pred$channel_group[i])
    # ch <- "android - FB App Install"
    ch_cost <- df_data %>%
      mutate(month=as.Date(month, "%m/%d/%Y")) %>%
      filter((as.character(channel) == ch) & !(is.na(cost))) %>%
      mutate(cost=cost*Total_budget/budget) %>%
      arrange(month) %>%
      tail(3)
    maxcost <- min(Total_budget, quantile(ch_cost$cost, 0.99)*1.2)
    mincost <- max(0, quantile(ch_cost$cost, 0.00)*0.8)
    initcost <- min(Total_budget, quantile(ch_cost$cost,0.5))
    # ub[i] <- maxcost; lb[i] <- mincost; init[i] <- initcost
    val <- df_pred[i,"budget_channel"]
    val <- ifelse(val>=maxcost, 0.99*maxcost, val)
    df_pred[i,"budget_channel"] <- val
  }
  return(df_pred)
}
