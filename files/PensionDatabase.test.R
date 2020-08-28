###Testing Reason Database
## by Anil

#Clean Global Environment
rm(list = ls())

### Load R Packages ###

#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
#install.packages("data.table")
library(pensionviewr)
library(tseries)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(plotly)
library(plyr)
library(dplyr)

#Detailed overview of `pensionviewr` package
#https://github.com/ReasonFoundation/pensionviewr

##### [1]PullStateData() #####

state.data <- pullStateData(1990)
#**Uncomment to use below

#str(state.data)
#View(state.data)
#View(colnames(state.data))
#View(unique(state.data$display_name))

#Filter by state
New_Mexico.data <- state.data %>% filter(state == 'New Mexico')
#View(New_Mexico.data)

# filter by plan
NMERB.data <- state.data %>% filter(display_name == "New Mexico Educational Retirement Board")
#View(NMERB.data)



##### [2]PullData() #####

pl <- planList()# pull list of states, plan ids, and plan names
#View(pl)

#Pull data for a specific plan from database
NMERB.data <- pullData(pl,"New Mexico Educational Retirement Board") %>% 
                        filter(year >= 2001)
#View(NMERB.data)
#View(colnames(NMERB.data))



##### [3]Manipulate Data #####

####
#Filter pulled or loaded data
NMERB.data.filtered <- filterData(NMERB.data, 2010)
#View(NMERB.data.filtered)


####
#Transform data from long to wide format 
#w/ pivot_wider (Tidyr)

#Run this function first
pullSource <- function(plan_name){
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  # define the query to retrieve the plan data
  
  if(str_count(plan_name)<6){
    query <- paste("select * from pull_data_state_only()
where year > '2001'
and attribute_name in ('1 Year Investment Return Percentage',
'1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'Actuarially Accrued Liabilities Dollar',
'Total Normal Cost Percentage',
'Covered Payroll Dollar',
'Payroll Growth Assumption',
'Total Benefits Paid Dollar')")}else{
  
  plan_id <- pl$id[pl$display_name == plan_name]
  query <- paste("select * from pull_plan_data(",plan_id,")")
  #paste0("select * from pull_plan_data('", str_replace(plan_name,"'", "''"), "')")
}
  
  result <- RPostgres::dbSendQuery(con, query)
  #RPostgres::dbBind(result, list(1))
  all_data <- RPostgres::dbFetch(result) %>%
    janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  
  all_data %>%
    dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%  # group by everything other than the value column.
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%  # build group index
    dplyr::select(-.data$row_id) %>%  # drop the index
    janitor::clean_names()
}

#Use this function for Hawaii pension plan
data <- pullSource("Employee Retirement System of Hawaii")
#View(data)

#Spread data from Long to Wide format
data <- data %>% tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value)

####
## Manipulate using lapply (sum values by column)

NMERB.data <- pullData(pl,"New Mexico Educational Retirement Board") %>% 
              filter(year >= 2001)
NMERB.data.cut <- data.table(NMERB.data %>% select(actuarial_value_of_assets_dollar, actuarially_accrued_liabilities_dollar))
#View(NMERB.data.cut)
NMERB.data.cut = NMERB.data.cut[,lapply(.SD,sum),.SDcols=colnames(NMERB.data.cut)]
#View(NMERB.data.cut)

####
## Manipulate using `data.table` (w/ by=list())
state.data <- data.table(pullStateData(2001))
state.data$actuarially_accrued_liabilities_dollar <- as.numeric(state.data$actuarially_accrued_liabilities_dollar)
state.data.aal <- state.data[, sum(na.omit(actuarially_accrued_liabilities_dollar)), 
                               by=list(year, state)] %>%
                               arrange(state)
#View(state.data.aal)

####
state.data$market_value_of_assets_dollar <- as.numeric(state.data$market_value_of_assets_dollar)

#Create funded ratio columns (w/ data.table & dplyr)
state.data <- state.data[,funded_mva := market_value_of_assets_dollar/actuarially_accrued_liabilities_dollar]
#Same with %>% mutate()
state.data <- state.data %>% mutate(funded_mva2 = market_value_of_assets_dollar/actuarially_accrued_liabilities_dollar)
#View(state.data)

#########
