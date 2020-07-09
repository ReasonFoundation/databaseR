###Load/install packages
library(reasontheme)
library(pensionviewr)
library(ggplot2)
library(tidyverse)
#library(janitor)
#library(openxlsx)
library(tseries)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(base64enc)
#devtools::install_github("ropensci/plotly")
library(dplyr)
library(plyr)

###TRUNCATED VERSION of the pull_data_state_only() 
###function for pulling state data from the dataabse

con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = "d629vjn37pbl3l",
  host = "ec2-3-209-200-73.compute-1.amazonaws.com",
  port = 5432,
  user = "reason_readonly",
  password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
  sslmode = "require")

query <- "select * from pull_data_state_only()
where year > '2000'
and attribute_name in ('1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'10 Year Investment Return Percentage', 
'Actuarially Accrued Liabilities Dollar',
'Total Normal Cost Percentage',
'Covered Payroll Dollar',
'Payroll Growth Assumption',
'Total Benefits Paid Dollar')"

result <- RPostgres::dbSendQuery(con, query)
RPostgres::dbBind(result, params = NULL)
all_data <- RPostgres::dbFetch(result) %>%
  janitor::clean_names()
RPostgres::dbClearResult(result)
RPostgres::dbDisconnect(con)

state_plan_data <- all_data
##
View(all_data)
##