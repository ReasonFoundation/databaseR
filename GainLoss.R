#### GainLoss Analysis ####
### by Anil
## Reason Database

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

#Pull state-level data from the database
pl <- planList()
#View(pl)
reason.data <- pullData(pl, "Louisiana State Employees Retirement System")
#View(colnames(reason.data))
#View(reason.data$amortization_payment_total_amount)

#Select existing Gain/Loss columns
reason.data <- data.table(
  reason.data  %>%
    filter(year > 2000) %>%
    select(year, display_name, state, 
           investment_experience_dollar, 
           interest_on_debt_dollar,
           other_actuarial_experience_dollar,
           legislative_changes_dollar,
           changes_to_methods_assumptions_dollar,
           gain_or_loss_due_to_changes_in_benefits,
           gain_or_loss_due_to_changes_in_cola_provisions,
           gain_or_loss_due_to_changes_in_pbi_provisions,
           #age_of_retirement_experience_dollar,
           #disability_claim_experience_total_dollar,
           #mortality_rate_experience_total_dollar,
           #new_entrant_experience_dollar,
           salary_experience_dollar,
           #payroll_experience_dollar,
           #withdrawal_experience_dollar,
           contribution_deficiency_dollar,
       ###Extra non-G/L columns
           amortization_payment_total_amount,
           unfunded_actuarially_accrued_liabilities_dollar)
)

#View(colnames(reason.data))

####
#Some columns are not filled (especially demographic (e.g. mortality, new entrants, disability, payroll))

#View(reason.data)
reason.data <- data.table(reason.data)

#Fill all NAs w/ 0
reason.data <- reason.data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

#Make sure all numbers are in numeric format
reason.data[,4:15] <- data.table(reason.data[,4:15]) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
reason.data$year <- as.numeric(reason.data$year)

#Add Net Amortization calculation 
#(Interest on Debt + Amortization Payments)
reason.data <- reason.data[,net_amo := (interest_on_debt_dollar + amortization_payment_total_amount)]
ual <- reason.data$unfunded_actuarially_accrued_liabilities_dollar
reason.data <- reason.data %>% select(!interest_on_debt_dollar & 
                                        !amortization_payment_total_amount & 
                                        !unfunded_actuarially_accrued_liabilities_dollar)

#View(reason.data %>%
#       summarise(
#         across(c(colnames(reason.data[,4:13])),  .fns = list(sum)))
#)

#Add Total column by Year
for(i in (1:(max(reason.data$year)-min(reason.data$year)))){
  reason.data$total[i] <- sum(reason.data[i,4:13])
  
}

#net Change to UAL for 2001+ period (in $Billions)
gl.change <- sum(reason.data[1:19]$total)/1000000000
ual.change <- (max(ual)-ual[2])/1000000000
#Difference between cumulative G/L and actual change in UAL (2002-2019)
(ual.change - gl.change)*1000 # in $Millions
############
############