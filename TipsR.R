### TipsR ###
## Data: Pension Database
# By: Anil

#Clean environment
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

#Pull state-level data from the database
reason.data <- pullStateData(2001)
#Filter data
reason.data <- data.table(filterData(reason.data, 2001))

#View(reason.data)

pl <- planList()
#filter for PERSI
PERSI.data<- reason.data %>% filter(plan_name == "Idaho Public Employee Retirement System")

#View(PERSI.data)

#Transforming from wide to long format w/ "melt"
###
PERSI.data.long <- data.table(melt(PERSI.data, id.vars="year"))
###
View(PERSI.data.long)

#### Tidyverse update ####
### dplyr 1.0

## Relocate()

## Select() is like filter
PERSI.data.long <- PERSI.data %>% 
  select(year, state, plan_name, return_1yr, arr, ava, aal)

#Convert column data to numeric
PERSI.data[,4:7] <- data.table(PERSI.data[4:7]) %>% dplyr::mutate_all(dplyr::funs(as.numeric))

#View(PERSI.data.long)

#Relocate() is like reordering columns
View(PERSI.data.long %>% 
     relocate(state, plan_name, year, return_1yr))

#Move one column before/after another column
View(PERSI.data.long %>% 
       relocate(state, .after = plan_name))

#Last column
View(PERSI.data.long %>% 
       relocate(state, .before = last_col()))

#Relocate by data type
View(PERSI.data.long %>% 
       relocate(where(is.numeric)))

#Relocate by letter
View(PERSI.data.long %>% 
       relocate(contains("v"), .before = year))

#### Manipulate data ####

PERSI.data.long <- data.table(melt(PERSI.data, id.vars="year"))

## data.table & dplyr

#1.data.table package 
#-- [, fns, by = list]
reason.data$arr <- as.numeric(reason.data$arr)
reason.data$ava <- as.numeric(reason.data$ava)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$return_1yr <- as.numeric(reason.data$return_1yr)

View(reason.data[, median(na.omit(arr)), by = list(year, state)])
View(reason.data[, median(na.omit(return_1yr)), by = list(year, state)])
View(reason.data[, sum(na.omit(ava))/sum(na.omit(aal)), by = list(year, state)])
View(reason.data)

#dplyr package
#-- across(variables, .fns)
View(reason.data %>%
  group_by(state, year) %>%
  summarise(
    across(c(arr, return_1yr),  .fns = list(median, sd)),
    .groups = "drop")
  )

#More complex summarization
View(reason.data %>%
       group_by(state, year) %>%
       summarise(
         across(
           c(arr, return_1yr),  
           .fns = list(
             "mean" = ~ mean(.x),
             "range lo" = ~ (mean(.x) - 2*sd(.x)),
             "range hi" = ~ (mean(.x) + 2*sd(.x))
           )),
         .groups = "drop")
)
### END