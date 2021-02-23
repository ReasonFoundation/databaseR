#### GainLoss Analysis ####
### by Anil & Swaroop
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

#View(pl <- planList())
#x <- pullStateData(2001)
#view <- masterView("Reason", TRUE)
#View(view)

##1. See what G/L columns consistently have data/most data
#Pull, but not use, "actuarial_experience_dollar" (aggregated)
##2. If Interest_on_Debt is not available? Calculate?
##3. If "amortization_payment_total_amount"is not available? Calculate?
##4. Do Excel G/L matches Database results & UAL change?

####
#Some columns are not filled (especially demographic (e.g. mortality, new entrants, disability, payroll))
#Look into the Code aggregating Survival, Mortality, Survival, Disability data for actives, retired, inactives, total
#Compare Net Change to UAL vs. Actual UAL change for 5-10 plans

#Pull plan-specific data from the database
pl <- planList()
#reason.data <- pullData(pl, "Dallas Police and Fire Pension System") %>% filter(year > 2000)# (2002-2020)

reason.data <- pullData(pl, "Louisiana State Employees Retirement System")# (2002-2019)
#reason.data <- pullData(pl, "New Mexico Public Employees Retirement Association")# (2002-2019)
#reason.data <- pullData(pl, "Teachers’ Retirement System of Louisiana")# (2002-2019)
#reason.data <- pullData(pl, "Arizona Public Safety Personnel Retirement System") (2002-2019)
#reason.data <- pullData(pl, "Georgia Teachers Retirement System")# (2002-2018)

#### Create a list of all Gain/Loss columns we have ####

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
                     "changes_to_methods_assumptions_dollar",
                     "interest_on_debt_dollar",
                     "other_interest_dollar",
                     "gain_or_loss_due_to_changes_in_benefits",
                     "gain_or_loss_due_to_changes_in_COLA_provisions",
                     "gain_or_loss_due_to_changes_in_pbi_provisions",
                     "gain_or_loss_due_to_changes_in_normal_cost_prior_year",
                     "gain_or_loss_due_to_changes_in_other_interest",
                     "contribution_deficiency_dollar",
                     #Non-G/L columns that may be empty
                     "amortization_payment_total_amount",
                     "total_amortization_payment_percentage",
                     "covered_payroll_dollar",
                     "fiscal_year_of_contribution",
                     "unfunded_actuarially_accrued_liabilities_dollar")

## How many columns are in the pulled data?
col1 <- ncol(reason.data)
col1

#### Save names of all the Gain/Loss columns that are currently not in the pulled data
columns2 <- c("test")
for (i in (1:length(gain.loss.names))) {
  if (sum(colnames(reason.data) %in% gain.loss.names[i]) == 0) {
    columns2 <- rbind(columns2, gain.loss.names[i])
  }
}

## Add these columns as empty ones (NA) to the original data
columns2 <- as.character(columns2 <- columns2[2:length(columns2)])
cols <- matrix(NA, length(reason.data[, 1]), length(columns2))
colnames(cols) <- columns2
reason.data <- cbind(reason.data, cols)
reason.data <- data.table(reason.data)

## Number of columns after we added missing G/L columns
col2 <- ncol(reason.data)
col2
#How many of the total 38 G/L columns were empty & added
col2-col1

## Filter for G/L columns only
reason.data <- reason.data %>%
  select(year, state, plan_name, gain.loss.names) %>%
  filter(year > 2000)

View(reason.data)
#View(masterView("Public Plans Database", TRUE))
#x <- masterView("Reason")
#x <- x %>% filter(plan_attribute_id >= 10798)
#10798-10829
#View(x$master_attribute_name[19:63])


## Fill all NAs w/ 0
reason.data <- data.table(reason.data)
reason.data <- reason.data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

## Make sure all column numbers are numeric
reason.data[,4:(length(reason.data)-1)] <- data.table(reason.data[,4:(length(reason.data)-1)]) %>% dplyr::mutate_all(as.numeric)
reason.data$year <- as.numeric(reason.data$year)

#### Add Net Amortization ####
#(Interest on Debt + Amortization Payments)
#Code alternative way>> 
#Amo. contribution % * Future Payroll
future.payroll <- reason.data[year >= fiscal_year_of_contribution[1]]$covered_payroll_dollar
future.payroll <- t(cbind(t(future.payroll),reason.data[year == 2020]$covered_payroll_dollar*(1+0.03)))#Use assumed payroll growth rate
reason.data$payroll2 <- future.payroll

## Calculate net amortization
reason.data <- reason.data[,net_amo := (interest_on_debt_dollar + 
                                      if(sum(reason.data$amortization_payment_total_amount)!=0){
                                        amortization_payment_total_amount}else{
                                        total_amortization_payment_percentage * payroll2})]

## Save actual UAL for future comparison w/ G/L                                                   
ual <- data.table(reason.data %>% select(year, state, plan_name, 
                                         unfunded_actuarially_accrued_liabilities_dollar))
#Remove columns used for Net Amortization
reason.data <- reason.data %>% select(!interest_on_debt_dollar & 
                                        !amortization_payment_total_amount & 
                                        !unfunded_actuarially_accrued_liabilities_dollar &
                                        !actuarial_experience_dollar &
                                        !total_amortization_payment_percentage &
                                        !covered_payroll_dollar &
                                        !fiscal_year_of_contribution &
                                        !payroll2)

View(reason.data)
#View(reason.data %>%
#       summarise(
#         across(c(colnames(reason.data[,4:13])),  .fns = list(sum)))
#)
##### Calculate Total G/L Change for Each Year ####
x <- length(reason.data)

for(i in (1:reason.data[,.N])){
  reason.data$total[i] <- sum(reason.data[i,4:x])
}

#View(reason.data$total)
## LASERS ONLY correction for Experience Account Disbursements
#reason.data[year > 2005 & year < 2009]$total <- c(-38272000, -34856000,343427000)
#reason.data[year > 2017 & year < 2020]$total <- c(-72193000, 263639000)
#View(reason.data[year > 2017 & year < 2020])

## Compare ##

#write.csv(reason.data, file = "DPF_GainLoss.csv", row.names = FALSE)
# #Aggregate Gain/Loss change for 2002+ period (in $Billions)
gl.change <- sum(reason.data[1:19]$total)/1000000000
gl.change

## Actual change in UAL for 2001+ period (in $Billions)
ual.change <- (max(ual[year == 2019]$unfunded_actuarially_accrued_liabilities_dollar)
               -ual[year == 2001]$unfunded_actuarially_accrued_liabilities_dollar)/1000000000
ual.change

## Difference between cumulative G/L  & actual change in UAL
round((ual.change - gl.change),4)*1000 # in $Millions
############
############

##Load PPD investment data
urlfile= "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/PensionInvestmentPerformanceDetailed_V1.csv"
data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types =       NULL)
data <- as.data.table(data)# convert to data.table

## Filter for columns ending w/ "_Actl"
data <- data %>%
  select("ppd_id",
         "PlanName",
         "EEGroupID",
         "TierID", 
         contains("_Actl"))

## Convert all numbers to numeric format
data[,5:83] <- data[,5:83] %>% mutate_all(as.numeric)

## Use loop to sum up all allocations (should be 1)
for (i in (2: length(data$total))){
  
  data$total[i] <- sum(data[i,5:83])  
  
}

View(data)
############


############
