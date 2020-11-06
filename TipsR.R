### TipsR ###
## Data: Pension Database
# By: Anil

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
#Shiny
library(shiny)
library(shinyWidgets)
#library(shinyFiles)
library(DT)
library(plotly)
library(rlang)
library(purrr)

#Pull state-level data from the database
reason.data <- pullStateData(2001)
#Filter data
reason.data <- data.table(filterData(reason.data, 2001))
#View(reason.data)
#Save 1st 3 columns
x <- reason.data[,1:3]

#Mutate all columns to "numeric" type
###
reason.data <- data.table(reason.data) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
###
reason.data[,1:3] <- x
#View(reason.data)

pl <- planList()

#filter for PERSI
PERSI.data<- reason.data %>% filter(plan_name == "Idaho Public Employee Retirement System")
View(PERSI.data)

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

## data.table & dplyr
#data.table -- [, fns, by = list]
View(reason.data[, median(na.omit(arr)), by = list(year, state)])

#dplyr -- across()
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
###