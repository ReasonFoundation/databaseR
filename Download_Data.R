##Working With Reason Database
#by Anil Niraula

###How to Download data###
rm(list=ls())
###Packages
library(reasontheme)
library(pensionviewr)
library(tidyverse)
library(tseries)
library(data.table)
library(readr)
library(rsconnect)
library(dplyr)
library(plyr)
#########

##Way [1]

######## Download State-Level Data from GitHub to R:
##Use a combination of url() + read_csv()
#RUN THIS:
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data <- data.table(reason.data)

##Way [2]

######## Download State-Level Data from database to R:
urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
##Packages: 
library(pensionviewr)#more info: https://github.com/ReasonFoundation/pensionviewr
library(dplyr)
##Use a combination of pullData() + filter() from dplyr
pl <- planList()
reason.data <- pullData(pl, plan.names[,1]) %>% arrange(state)
reason.data <- data.table(reason.data)

##Way [3]

######## Download All Data (state & local) from database to R:
##Use a combination of pullData() + filter() from dplyr
pl <- planList()
reason.data <- pullData(pl, pl$display_name) %>% arrange(state)
reason.data <- data.table(reason.data)
#Filter for state-level plans if needed
reason.data <- reason.data[administering_government_type == 0]

##Way [4]

######## Download Plan-By-Plan Data from database to R:
#Go through Ways 1-3, and then:
myplan.data <- reason.data[display_name == "Arkansas Teachers Retirement Plan"]
#OR
#-- download directly from the database:
pl <- planList()
myplan.data <- pullData(pl, "Arkansas Teachers Retirement Plan")
myplan.data <- data.table(reason.data)