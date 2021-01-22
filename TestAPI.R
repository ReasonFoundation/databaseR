#https://publicplansdata.org/public-plans-database/api/#examples
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/API-data-access-r/
library(httr)
library(grid)
library(tidyverse)
library(pensionviewr)
#library(openxlsx)
library(tseries)
library(plyr)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny
library(shiny)
library(shinyWidgets)
#library(shinyFiles)
library(DT)
library(plotly)
library(rlang)
library(purrr)
library(rpart)
library(vip)
library(stringr)

#Load PPD variable list
var.list <- read_csv(url(
  "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/Variable-List.csv"), 
              col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

y <- var.list[1:100,3]
y <- data.table(y)
y <- y[!7:8,]
y <- y[!15,]
y <- y[!19,]
y <- y[!26,]
y <- y[!26:29,]
y <- y[!32:35,]
y <- y[!37:52,]
y <- y[!38,]
y <- y[!40,]
y <- y[!44,]
y <- y[!48,]
y <- y[!51,]
y <- y[!52:53,]
y <- y[!55:57,]
View(y)
View(masterView("Pulic Plans Database"))
y <- str_replace_all(y, "[ \"c()]" , "")

#Set starting/emding years
startYear <- 2001
endYear <- 2020
y
#Set custom url address with filtered years and variables
url_ppd <- paste0("http://publicplansdata.org/api/?q=QVariables&variables=",
y,
"&filterfystart=",paste0(startYear),"&filterfyend=",paste0(endYear), collapse=",")

#Remove "\n" parts generated for commas
url_ppd <- str_replace_all(url_ppd, "[\r\n]" , "")
url_ppd

x <- read_csv(url(url_ppd), 
  col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
x <- data.table(x)
View(x)
#repos$date