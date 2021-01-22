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

View(pl <- planList())
x <- pullStateData(2001)
#view <- masterView("Reason", TRUE)
#View(view)

##1. See what G/L columns consistently have data/most data
#Pull, but not use, "actuarial_experience_dollar" (aggregated)
##2. If Interest_on_Debt is not available? Calculate?
##3. If "amortization_payment_total_amount"is not available? Calculate?
##4. Do Excel G/L matches Database results?

#Pull state-level data from the database
pl <- planList()
#View(pl)
reason.data <- pullSourceData(pl, "Louisiana State Employees Retirement System", 2001)# (2002-2019)

#reason.data <- pullData(pl, "New Mexico Public Employees Retirement Association")# (2002-2019)
#reason.data <- pullData(pl, "Teachers’ Retirement System of Louisiana")# (2002-2019)
#reason.data <- pullData(pl, "Arizona Public Safety Personnel Retirement System") (2002-2019)
#reason.data <- pullData(pl, "Georgia Teachers Retirement System")# (2002-2018)

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
           #other_interest_dollar,
           #gain_or_loss_due_to_changes_in_normal_cost_prior_year,
           #gain_or_loss_due_to_changes_in_other_interest,
           #age_of_retirement_experience_dollar,
           #disability_claim_experience_active_dollar,
           #disability_claim_experience_inactive_dollar,
           #disability_claim_experience_retired_dollar,
           #disability_claim_experience_total_dollar,
           #mortality_rate_experience_active_dollar,
           #mortality_rate_experience_inactive_dollar,
           #mortality_rate_experience_retired_dollar,
           #mortality_rate_experience_total_dollar,
           #survival_claim_experience_active_dollar,
           #survival_claim_experience_inactive_dollar,
           #survival_claim_experience_retired_dollar,
           #survival_claim_experience_total_dollar",
           #new_entrant_experience_dollar,
           salary_experience_dollar,
           #payroll_experience_dollar,
           #withdrawal_experience_dollar,
           #rehire_experiennce_dollar,
           contribution_deficiency_dollar,
           #interest_smoothing_dollar,
       ###Extra non-G/L columns
           amortization_payment_total_amount,
           unfunded_actuarially_accrued_liabilities_dollar)
)

#View(masterView("Public Plans Database"))
#View(masterView("Public Plans Database", TRUE))
x <- masterView("Reason")
x <- x %>% filter(plan_attribute_id >= 10798)
#10798-10829
#View(x$master_attribute_name[19:63])

gain.loss.names <- c("investment_experience_dollar",
                     "age_of_retirement_experience_dollar",
                     "disability_claim_experience_active_dollar",
                     "disability_claim_experience_inactive_dollar",
                     "disability_claim_experience_retired_dollar",
                     "disability_claim_experience_total_dollar",
                     "mortality_rate_experience_active_dollar",
                     "mortality_rate_experience_inactive_dollar",
                     "mortality_rate_experience_retired_dollar",
                     "mortality_rate_experience_total_dollar",
                     "survival_claim_experience_active_dollar",
                     "survival_claim_experience_inactive_dollar",
                     "survival_claim_experience_retired_dollar",
                     "survival_claim_experience_total_dollar",
                     "withdrawal_experience_dollar",
                     "salary_experience_dollar",
                     "payroll_experience_dollar",
                     "new_entrant_experience_dollar",
                     "rehire_experiennce_dollar",
                     "other_actuarial_experience_dollar",
                     "interest_smoothing_dollar",
                     "non_investment_actuarial_experience_dollar",
                     "actuarial_experience_dollar",
                     "legislative_changes_dollar",
                     "changes_to_methods_&_assumptions_dollar",
                     "interest_on_debt_dollar_dollar",
                     "gain_or_loss_due_to_changes_in_benefits",
                     "gain_or_loss_due_to_changes_in_COLA_provisions",
                     "gain_or_loss_due_to_changes_in_PBI_provisions",
                     "gain_or_loss_due_to_changes_in_normal_cost_prior_year",
                     "gain_or_loss_due_to_changes_in_other_interest",
                     "contribution_deficiency_dollar")
#View(colnames(reason.data))

####
#Some columns are not filled (especially demographic (e.g. mortality, new entrants, disability, payroll))
#Look into the Code aggregating Survival, Mortality, Survival, Disability data for actives, retired, inactives, total
#Compare Net Change to UAL vs. Actual UAL change for 5-10 plans

#View(reason.data)
reason.data <- data.table(reason.data)

#Fill all NAs w/ 0
reason.data <- reason.data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

#Make sure all numbers are in numeric format
reason.data[,4:15] <- data.table(reason.data[,4:15]) %>% dplyr::mutate_all(as.numeric)
reason.data$year <- as.numeric(reason.data$year)

#Add Net Amortization calculation 
#(Interest on Debt + Amortization Payments)
reason.data <- reason.data[,net_amo := (interest_on_debt_dollar + amortization_payment_total_amount)]
View(reason.data)
ual <- reason.data$unfunded_actuarially_accrued_liabilities_dollar
#View(reason.data)
reason.data <- reason.data %>% select(!interest_on_debt_dollar & 
                                        !amortization_payment_total_amount & 
                                        !unfunded_actuarially_accrued_liabilities_dollar)

#View(reason.data %>%
#       summarise(
#         across(c(colnames(reason.data[,4:13])),  .fns = list(sum)))
#)

#Calculate Total G/L by Year
for(i in (1:(max(reason.data$year)-min(reason.data$year)))){
  reason.data$total[i] <- sum(reason.data[i,4:13])
}
#View(reason.data)
#View(ual)
#Net Change to UAL for 2001+ period (in $Billions)
gl.change <- sum(reason.data[2:19]$total)/1000000000
gl.change
#Actual change in UAL for 2001+ period (in $Billions)
ual.change <- (max(ual)-ual[1])/1000000000
max(ual)-ual[1]
#Difference between cumulative G/L (2002-2019) & actual change in UAL (2001-2019)
round((ual.change - gl.change),4)*1000 # in $Millions
############
############