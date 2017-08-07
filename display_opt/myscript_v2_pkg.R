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

# file_historical <- "./data_input_historical_cost.csv"
# file_budseas <- "./data_input_budgetseason.csv"
# param_month <- as.Date("2017-07-01","%Y-%m-%d")
# param_season <- 0
# param_budget <- 6.858

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

# model <- createModel(file_historical = file_historical, file_budseas = file_budseas)
