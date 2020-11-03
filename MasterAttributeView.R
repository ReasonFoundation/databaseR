rm(list=ls())
###Load/install packages
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(grid)#https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
library(tidyverse)
#library(openxlsx)
library(tseries)
library(plyr)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
#library(shinyFiles)
library(DT)
library(plotly)
#devtools::insta


##View master attribute names/ids
con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = "d629vjn37pbl3l",
  host = "ec2-3-209-200-73.compute-1.amazonaws.com",
  port = 5432,
  user = "reason_readonly",
  password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
  sslmode = "require")

# define the query to retrieve the plan data
query <- paste("select * from master_priority_view")

#Send query & get the data
result <- RPostgres::dbSendQuery(con, query)
#RPostgres::dbBind(result, list(1))
all_data <- RPostgres::dbFetch(result) %>%
  janitor::clean_names()
RPostgres::dbClearResult(result)
RPostgres::dbDisconnect(con)

View(all_data)
all_data <- data.table(all_data)
##############

##
View(all_data$attribute_name)
all_data <- data.table(all_data)
reason_data <- all_data[data_source_name == "Reason"]
write.csv(reason_data, file = "/Users/anilniraula/Downloads/reason_data.csv")
##############