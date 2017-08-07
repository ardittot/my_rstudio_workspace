library(datarobot)
#ConnectToDataRobot(token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N', endpoint ='http://data-datarobot-apps-01.prod.data.tvlk.cloud/api/v2')
ConnectToDataRobot(token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N', endpoint ='http://tvlk-data-datarobot-apps-lb-1080587125.ap-southeast-1.elb.amazonaws.com/api/v2')

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
  return(res)
}

trainDRModel <- function(df, response_var){
  lcol <- c("channel","tier","budget","cost",paste(response_var,"_target",sep = ""),response_var)
  df1 <- df[,lcol]
  df1 <- df1 %>% filter(!is.na(cost))
  project_name <- paste("MTA: Display Opt -",response_var)
  project <- SetupProject(dataSource = df1, projectName = project_name)
  SetTarget(project = project, target = response_var)
  WaitForAutopilot(project = project)
  return(project)
}

getDRProject <- function(response_var){
  proj_name <- paste("MTA: Display Opt -",response_var)
  proj_id <- GetProjectList()$projectId[GetProjectList()$projectName %in% proj_name]
  proj <- GetProject(proj_id)
  return(proj)
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
  return(res)
}


