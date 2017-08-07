rm(list=ls())

library(forecast)
library(tseries)
library(TTR)
library(ggplot2)
library(xts)
require(gridExtra)
require(lmtest)
library(lubridate)
require(astsa)
library(bsts)
library(reshape)
library(dplyr)

savefile_old <-  "./timeseries/savefile2016.csv"
savefile <- "./timeseries/savefile.csv"
file_budseas <- "./data_input_budgetseason.csv"
param_level_season <- 0
param_budget <- 6.858

#### Seasonality & Budget ####
df_budseas <- read.csv(file_budseas, header = TRUE)
df_budseas <- df_budseas %>%
  mutate(channel=(paste(platform,"-",channel_group))) %>%
  mutate(channel=factor(channel)) %>%
  mutate(month=as.Date(month)) %>%
  select(month,channel,season,budget)
# -------------- #
# df_budseas$channel <- strsplit(as.character(df_budseas$channel),split=' - ', fixed=TRUE)
# df_budseas$platform <- ""
# df_budseas$channel_group <- ""
# for (i in 1:nrow(df_budseas)){
#   df_budseas[i,"platform"] <- df_budseas[,"channel"][[i]][1]
#   df_budseas[i,"channel_group"] <- df_budseas[,"channel"][[i]][2]
# }
# df_budseas <- df_budseas %>% select(month,platform,channel_group,season,budget)
# write.csv(df_budseas, "./data_input_budgetseason.csv", row.names = FALSE)
# -------------- #

#### Historical Data ####
df_orig <- read.csv(savefile,header=TRUE)
# Only temporary #
df_orig_old <- read.csv(savefile_old,header=TRUE)
df_orig_old <- df_orig_old %>% dplyr::rename(reporting_date=date, total_cost_usd=cost_usd) %>% select(reporting_date, platform, channel_group, total_cost_usd)
df_orig <- rbind(df_orig_old,df_orig)
# -------------- #
# df_old <- df_orig_old %>%
#   mutate(channel=paste(platform,"-",channel_group)) %>%
#   select(date,channel,cost_usd) %>%
#   dplyr::rename(cost=cost_usd)
# df <- df_orig %>%
#   mutate(channel=paste(platform,"-",channel_group)) %>%
#   select(reporting_date,channel,total_cost_usd) %>%
#   dplyr::rename(cost=total_cost_usd, date=reporting_date)
# df <- rbind(df_old, df)
# -------------- #

df <- df_orig %>%
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
  select(month,channel,season,budget_prop,budget,cost)

## Check completeness of each channel data
check_completeness <- reshape2::dcast(cbind(df, cnt=rep(1,nrow(df))), value.var = "cnt", month ~ channel, sum)

data_train <- df[df$month<max(df$month),]
data_test <- df[df$month>=max(df$month),]
# cost_pred <- read.csv("./prediction_result.csv")$Prediction

y <- xts(data_train[,"cost"],order.by=data_train[,"month"])
colnames(y) <- c("cost")
# Seasonal component of num_trx
seas <- ts(na.omit(data_train$cost), frequency=12)
decomp <- stl(seas, s.window="periodic")
y_ss <- decomp$time.series[,"trend"] + decomp$time.series[,"seasonal"]

ss <- list()
ss <- AddLocalLevel(ss, y_ss)
# ss <- AddLocalLinearTrend(ss, y_ss)
# ss <- AddSeasonal(ss, y_ss, nseasons = 12)
model <- bsts(y ~ (. - month - cost - 1), state.specification = ss, niter = 4000, seed=99, ping = 4000/10, data = data_train)

# pred <- predict(model, burn = 100, quantiles = c(0.25, 0.75), newdata = data_test[1,])
data_actual <- select(data_test, month,channel)
data_actual$cost <- data_test$cost
cost_pred <- c()
for (ch in data_test$channel){
  toPred <- filter(data_test, channel==ch)
  pred <- predict(model, burn = 100, quantiles = c(0.25, 0.75), newdata = toPred)
  res <- pred$median
  cost_pred <- c(cost_pred, res)
}
data_result <- cbind(data_test[,c("month","channel")], cost_pred)
data_result <- data_result %>% left_join(data_actual, by = c("month","channel"))
colnames(data_result) <- c("month","channel","cost_pred","cost_actual")

# write.csv(data_test, file = "./DR_ts_testing.csv", row.names = FALSE)
# write.csv(data_train, file = "./DR_ts_training.csv", row.names = FALSE)

# x <- seq(0,16987.33,100)
# Y <- (453.121408 + (x*0.057847))/(1 + ((3.7653*10^(-6))*x) + ((-3.111693*10^(-11))*(x)^(2)))
# fit <- loess(Y~x)
# plot(x,Y); lines(x,fit$fitted, col='red'); dev.off()


m