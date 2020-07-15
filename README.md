##ReadMe
##Working With Reason Database
#by Anil Niraula

#This folder contains files for filtering Reason database.
#Some of the files contain state-level plan data, others state pension plan names in the datatabse.
#The purpose is to upload these files to GitHub & then load them directly to R to optimize analysis and Shiny App syntax.

##############################
###How to Download Reason data

##Way [1]

######## Download State-Level Data from GitHub to R:
##Use a combination of url() + read_csv()
#RUN THIS:
#FILE LAST UPDATED: 07.15.20
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data <- data.table(reason.data)

##Way [2]

######## Download All Data (state & local) from database to R:
##Use a combination of pullData() + filter() from dplyr
pl <- planList()
reason.data <- pullData(pl, pl$display_name) %>% arrange(state)
reason.data <- data.table(reason.data)
#Filter for state-level plans if needed
reason.data <- reason.data[administering_government_type == 0]

##Way [3]

######## Download Plan-By-Plan Data from database to R:
#Go through Ways 1-3, and then:
myplan.data <- reason.data[display_name == "Arkansas Teachers Retirement Plan"]
#OR
#-- download directly from the database:
pl <- planList()
myplan.data <- pullData(pl, "Arkansas Teachers Retirement Plan")
myplan.data <- data.table(reason.data)
