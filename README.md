##ReadMe
##Working With Reason Database
#by Anil Niraula

#This folder contains files for filtering Reason database.
#Some of the files contain state-level plan data, others state pension plan names in the datatabse.
#The purpose is to upload these files to GitHub & then load them directly to R to optimize analysis and Shiny App syntax.

##############################
###How to Download Reason data

Way [1]
######## Download state-level data from "GitHub":
##Use a combination of url() + read_csv()

urlfile="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

Way [2]
######## Download the whole database from "R":
##Use a combination of pullData() + filter() from dplyr
##Packages: 
library(pensionviewr)#more info: https://github.com/ReasonFoundation/pensionviewr
library(dplyr)

reason.data  <- pullData(pl, pl$display_name)) %>% arrange(state)
#To filter for state-level plans
reason.data <- reason.data %>% filter(administering_government_type == 0)
