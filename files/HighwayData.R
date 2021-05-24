rm(list=ls())
###Load/install packages
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr_private",  ref = "master", auth_token = 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)

library(reasontheme)
library(pensionviewr)
library(ggplot2)
library(tidyverse)
library(tseries)
library(data.table)
library(readr)
library(rsconnect)
library(dplyr)
library(plyr)

### Load Highway data
library(readxl)
library(fs)
Final_AHR_Report_2021_ex <- read_excel("~/Downloads/Final_AHR_Report 2021.xlsx", sheet = 1)
Final_AHR_Report_2021_ex <- data.frame(Final_AHR_Report_2021_ex[2:51,])
#View(Final_AHR_Report_2021_ex)

## Structure
str(Final_AHR_Report_2021_ex)

## Skim (Quick information for each variable)
library(skimr)
skim(Final_AHR_Report_2021_ex)
#View(colnames(Final_AHR_Report_2021_ex))

##Graph Miles vs. Capital Disbursements
plot(Final_AHR_Report_2021_ex$State.Controlled.Lane.Miles, Final_AHR_Report_2021_ex$Capital.Disbursements)

#### Linear Positive Relationship between Miles & C. Disbursements
###############
ggplot(Final_AHR_Report_2021_ex,
       aes(x = State.Controlled.Lane.Miles, 
           y = Capital.Disbursements)
)+geom_point() + geom_smooth(method = "lm")
##use geom_smooth(method = "auto") to identify non-linear relationship

## Make sure all columns are numeric
###############
Final_AHR_Report_2021_ex[,5:51] <- Final_AHR_Report_2021_ex[,5:51] %>%
  mutate_all(as.numeric)
Final_AHR_Report_2021_ex <- data.table(Final_AHR_Report_2021_ex)
Final_AHR_Report_2021_ex <- Final_AHR_Report_2021_ex %>%
  replace(is.na(.), 0)

############### Regression: Capital Disbursements on Miles, Fatalities & Poor Rural Condition
summary(
  lm(Final_AHR_Report_2021_ex$Capital.Disbursements ~ 
      Final_AHR_Report_2021_ex$State.Controlled.Lane.Miles+
       Final_AHR_Report_2021_ex$Total.Fatalities + 
       Final_AHR_Report_2021_ex$Rural.Interstate.Poor.Condition...35
     )
)

############### Regression: Poor Rural Condition on C. Disbursements
ggplot(Final_AHR_Report_2021_ex,
       aes(x = Rural.Interstate.Poor.Condition...35, 
           y = Capital.Disbursements))+geom_point() + geom_smooth(method = "auto")

#State.Controlled.Lane.Miles       
summary(lm(Final_AHR_Report_2021_ex$Rural.Interstate.Poor.Condition...35~
             Final_AHR_Report_2021_ex$Capital.Disbursements))

## See Correlation Table

View(
  cor(Final_AHR_Report_2021_ex[,5:51])
)
#############

###### Other Distribution (BoxPlots)

boxplot(Final_AHR_Report_2021_ex$SHA.Miles)
box <- boxplot(Final_AHR_Report_2021_ex$SHA.Miles)
box
## Outlier States
box$out

summary(Final_AHR_Report_2021_ex$SHA.Miles)
hist(Final_AHR_Report_2021_ex$Total.Fatalities)
#hist(Final_AHR_Report_2021_ex$Capital.Disbursements)
#hist(Final_AHR_Report_2021_ex$State.Controlled.Lane.Miles)

### See top 10 states on C. Disbursements etc
View(Final_AHR_Report_2021_ex %>% slice_max(Final_AHR_Report_2021_ex$Capital.Disbursements, n = 10))
View(Final_AHR_Report_2021_ex %>% slice_max(Final_AHR_Report_2021_ex$SHA.Mile, n = 10))
######
######