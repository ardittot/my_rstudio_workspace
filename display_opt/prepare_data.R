download.s3.csv <- function(s3_bucket, source_loc, target_loc) {
  cmdstr <- sprintf('aws s3 cp --profile %s %s %s', s3_bucket, source_loc, target_loc)
  system(cmdstr)
}

# download.s3.csv('tvlk-data-databricks-dev', 's3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/data_input_historical_cost.csv', './data/data_input_historical_cost.csv')
# download.s3.csv('tvlk-data-databricks-dev', 's3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/data_input_budget_season.csv', './data/data_input_budget_season.csv')
# download.s3.csv('tvlk-data-databricks-dev', 's3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/data_input_datarobot.csv', './data/data_input_datarobot.csv')
# download.s3.csv('tvlk-data-databricks-dev', 's3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/data_input_to_pred.csv', './data/data_input_to_pred.csv')

download.s3.csv('tvlk-data-databricks-dev', paste('s3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/',file_input_historical_cost,sep=""), file_historical)
download.s3.csv('tvlk-data-databricks-dev', paste('s3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/',file_input_budget_season,sep=""), file_budget_season)
download.s3.csv('tvlk-data-databricks-dev', paste('s3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/',file_input_DR_dataset,sep=""), file_DR_dataset)
download.s3.csv('tvlk-data-databricks-dev', paste('s3://tvlk-data-databricks-dev/at_playground/MTA_Display_Opt/prod/',file_input_to_pred,sep=""), file_to_pred)
