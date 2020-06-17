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

######## Download state-level data from GitHub to R:
##Use a combination of url() + read_csv()
#RUN THIS:
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data <- data.table(reason.data)

##Way [2]

######## Download All data from database to R:
##Use a combination of pullData() + filter() from dplyr
##Packages: 
library(pensionviewr)#more info: https://github.com/ReasonFoundation/pensionviewr
library(dplyr)
#RUN THIS:
pl <- planList()
reason.data <- pullData(pl, pl$display_name) %>% arrange(state)
reason.data <- data.table(reason.data)
reason.data <- reason.data[administering_government_type == 0]#To filter for state-level plans
#unique(reason.data$type_of_employees_covered) #look up employees covered
#"Plan covers state and local employees"
#"Plan covers teachers"
#"Plan covers police and/or fire"       
#"Plan covers local employees"
#"Plan covers state employees"
#View(reason.data[type_of_employees_covered == "Plan covers teachers"]) #look up employees covered

##Way [3]

######## Download Plan-By-Plan data from database to R:
#Go through Way 1 or 2, and then:
myplan.data <- reason.data[display_name == "Arkansas Teachers Retirement Plan"]
#OR -- download directly from the database:
pl <- planList()
myplan.data <- pullData(pl, "Arkansas Teachers Retirement Plan")
myplan.data <- data.table(reason.data)