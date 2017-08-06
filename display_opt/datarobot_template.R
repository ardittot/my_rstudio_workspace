library(datarobot)
#ConnectToDataRobot(endpoint ='http://data-datarobot-apps-01.prod.data.tvlk.cloud/api/v2', token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N')
#ConnectToDataRobot(token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N', endpoint ='http://data-datarobot-apps-01.prod.data.tvlk.cloud/api/v2')
#ConnectToDataRobot(endpoint ='http://tvlk-data-datarobot-apps-lb-1080587125.ap-southeast-1.elb.amazonaws.com/api/v2', token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N')
ConnectToDataRobot(token='OIrd03JlWbo4q42PASFdHztbC5rmYe0N', endpoint ='http://tvlk-data-datarobot-apps-lb-1080587125.ap-southeast-1.elb.amazonaws.com/api/v2')

fixChannelName <- function(df){
  res <- rep("",length(df))
  for (i in 1:length(df)){
    x <- as.character(df[i])
    spl <- strsplit(x, "-")[[1]]
    res[i] <- paste(spl[1]," - ",spl[2])
  }
  # res <- factor(res)
  return(res)
}

df <- read.csv("./datarobotfile.csv")
df$Channel <- fixChannelName(df$Channel)
df <- df %>% 
  # mutate(Date=as.Date(Date)) %>%
  # mutate(Channel=fixChannelName(Channel)) %>%
  mutate(Cost=as.numeric(as.character(Cost))) %>%
  mutate(CA.hotel.target=as.integer(as.character(CA.hotel.target))) %>%
  mutate(CA_flight=as.integer(as.character(CA_flight))) %>%
  mutate(CA_hotel=as.integer(as.character(CA_hotel))) %>%
  mutate(Sum_Install=as.integer(as.character(Sum_Install))) %>%
  mutate(Tier=factor(Tier,levels=c("NORMAL","HIGH"))) %>%
  select(-c(CA_flight_log,CA_hotel_log,Budget_log,Cost_log)) %>%
  # filter(Cost!="#N/A") %>%
  dplyr::rename(CA_flight_target=CA.flight.target, CA_hotel_target=CA.hotel.target, Total_budget=Total.Budget) %>%
  select(c(2,1,10,3:9))
# write.csv(df, "./data_input_datarobot.csv", row.names = FALSE)

df_flight <- df %>%
  select(Channel, Tier, Total_budget, Cost, CA_flight_target, CA_flight) %>%
  filter(Cost!="#N/A")

project <- SetupProject(dataSource = df_flight, projectName = "MTA: Display Opt - CA Flight")
SetTarget(project = project, target = "CA_flight")
WaitForAutopilot(project = project)

listOfModels <- GetAllModels(project)
summary(listOfModels)
plot(listOfModels, orderDecreasing = TRUE)

modelFrame <- as.data.frame(listOfModels)
modelType <- modelFrame$modelType
metric <- modelFrame$validationMetric
bestModelType <- modelType[which.min(metric)]
worstModelType <- modelType[which.max(metric)]
# modelFrame$expandedModel
bestIndex <- which.min(metric)
bestModel <- listOfModels[[bestIndex]]

dataset <- UploadPredictionDataset(project, df_flight)
bestPredictJobId <- RequestPredictionsForDataset(project, bestModel$modelId, dataset$id)
bestPredictions <- GetPredictions(project, bestPredictJobId)

