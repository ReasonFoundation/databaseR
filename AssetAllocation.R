#### Asset Allocation ####
### by Anil
## Source: PPD

rm(list=ls())

###Load/install packages

#To install `reasontheme` & `pensionviewr` packages you first want to load `devtools`
#install.packages('devtools')
#library(devtools)

#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(grid)
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

##Load PPD investment data
urlfile= "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/PensionInvestmentPerformanceDetailed_V1.csv"
data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types =       NULL)
data <- as.data.table(data)# convert to data.table

## Filter for columns ending w/ "_Actl"
data <- data %>%
  select("ppd_id",
         "PlanName",
         "fy",
         #"EEGroupID",
         #"TierID", 
         contains("_Actl"))

## Convert all numbers to numeric format
data[,4:82] <- data[,4:82] %>% mutate_all(as.numeric)
data$total <- 0

## Use loop to sum up all allocations (should be 1)
for (i in (2: length(data$total))){
  
  data$total[i] <- sum(data[i,4:82])  
  
}


## Example filtering for 1 Plan + Remove All Zero Columns
data <- data %>%
       filter(PlanName == "New Mexico PERA")

## Custom function to filter for value More than 0
all_zero <- function(x) any(x>0)
data <- data %>% select_if(all_zero)
View(data)
############
############