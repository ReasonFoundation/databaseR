#### TipsR ####
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
library(vip)

#Pull state-level data from the database
pl <- planList()
View(pl)
az.eorp <- pullData(pl, "Arizona Elected Officials Retirement Plan") %>% filter(year >=2001)
View(az.eorp)
#View(pl)
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data,2001)
reason.data <- reason.data %>% filter(year == 2019)
reason.data <- reason.data %>% select(year, state, plan_name
  #,adec
  #,adec_paid_pct
  ,statutory_pct
  #,er_nc_pct
  ,er_proj_adec_pct
  ,ee_nc_pct
  ,total_amortization_payment_pct
  ,total_nc_pct
  ,total_proj_adec_pct)
  
View(reason.data)


contributions_2019 <- DT::datatable(returns_2020, escape = FALSE, filter = "top", editable = FALSE, 
                              options = list(pageLength = 40, autoWidth = TRUE, dom = 't'), rownames= FALSE) %>% 
  #escape = FALSE keeps the a href links
  #filter = "top" add quick search bars below column titles
  #dom = 't' removes list and search bars at the top
  
  formatStyle("FY19-20 Returns", 
              #background = styleColorBar(returns_2020[,3], palette_reason$LightBlue),
              #backgroundSize = '98% 88%',
              #backgroundRepeat = 'no-repeat',
              #backgroundPosition = 'right',
              fontWeight = styleEqual(10, "bold")) %>%
  
  formatPercentage(c("FY19-20 Returns", "Assumed Rate of Return"),2) %>%
  
  formatCurrency(c("Market Assets (Billions)", "Estimated Investment Loss (Billions)"))

#adec_paid_pct
#statutory_pct
#ee_nc_pct
#er_nc_pct
#er_proj_adec_pct
#total_amortization_payment_pct
#total_nc_pct
#total_proj_adec_pct

reason.data <- reason.data %>% filter(!is.na(funded_ratio))

reason.data$arr <- as.numeric(reason.data$arr)
reason.data$year <- as.numeric(reason.data$year)
reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)

reason.data <- reason.data %>% 
  group_by(year, state) %>%
  transmute(funded.median = median(na.omit(funded_ratio)))

ggplot(reason.data, aes(x = year, y = funded.median, color = state)) +
  geom_line()


########
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data,2001)
reason.data <- reason.data %>% filter(!is.na(arr))

reason.data$arr <- as.numeric(reason.data$arr)
reason.data$year <- as.numeric(reason.data$year)
reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)

reason.data <- reason.data %>% 
  group_by(year, type_of_employees_covered) %>%
  transmute(arr.median = median(na.omit(arr)))

ggplot(reason.data, aes(x = year, y = arr.median, color = type_of_employees_covered)) +
  geom_line()

View(reason.data)
#View(colnames(reason.data))
#View(reason.data$amortization_payment_total_amount)

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
           amortization_payment_total_amount)
)

#View(colnames(reason.data))

####
#Some columns are not filled (especially demographic (e.g. mortality, new entrants, disability, payroll))

View(reason.data)
reason.data <- data.table(reason.data)
#Fill all NAs w/ 0
reason.data <- reason.data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
#Make sure all numbers are in numeric format
reason.data[,4:14] <- data.table(reason.data[,4:14]) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
reason.data$year <- as.numeric(reason.data$year)
reason.data <- reason.data[,net_amo := (interest_on_debt_dollar + amortization_payment_total_amount)]
reason.data <- reason.data %>% select(!interest_on_debt_dollar & !amortization_payment_total_amount)

#View(reason.data %>%
#       summarise(
#         across(c(colnames(reason.data[,4:13])),  .fns = list(sum)))
#)

#Add Total column by Year
for(i in (1:(max(reason.data$year)-min(reason.data$year)))){
  reason.data$total[i] <- sum(reason.data[i,4:13])
  
}
sum(reason.data[1:19]$total)/1000000000

View(masterView("Reason"))
#investment_experience_dollar


#Filter for state and local plan data for 2014 period

reason.data <- data.table(
  filterData(reason.data, 2014, "state and local")
  )

#Keep plans w/ statutory contributions
reason.data <- data.table(
  reason.data[!is.na(statutory)] %>%
    select(year, plan_name, state, adec, adec_paid_pct, statutory, statutory_pct)
  )
#View(reason.data)

#
View(masterView("Public Plans Database"))
View(masterView("Public Plans Database", TRUE))
x <- masterView("Reason")
x <- x %>% filter(plan_attribute_id >= 10798)
#10798-10829

View(x$master_attribute_name[19:63])

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

#Check PPD "RequiredContribution" vs. Statutory for 2015+ period for 2-3 plans that we don;t Have Reason data yet (e.g. Wyoming)
pl <- planList()

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


#############
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
library(gridExtra)
library(ggpubr)
library(httr)
library(jsonlite)

#Pull state-level data from the database
####
pl <- planList()

##Pull pension data from database
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data,2001)

##Filter for all Teacher plans
View(reason.data %>% 
       filter(type_of_employees_covered == "Plan covers teachers", year == 2019)
     %>% select(year, state, plan_name,type_of_employees_covered)
)


blended.teach <- c(
  "Arizona State Retirement System",
  "Delaware State Employees’ Pension Plan",
  "District of Columbia Teachers Retirement Fund",
  "Florida Retirement System",
  "Employee Retirement System of Hawaii",
  "Idaho Public Employee Retirement System",
  "Iowa Public Employees' Retirement System",
  "Kansas Public Employees' Retirement System",
  "Public Employees' Retirement System of Mississippi",
  "Nevada Public Employees Retirement System",
  "New Hampshire Retirement System",
  "North Carolina Teachers' and State Employees' Retirement System",
  "Oregon Public Employees Retirement System",
  "Rhode Island Employees Retirement System",
  "South Carolina Retirement Systems",
  "South Dakota Retirement System",
  "Tennessee Consolidated Retirement System, Teachers Pension Plan",
  "Utah Retirement Systems, Noncontributory Retirement System",
  "Virginia Retirement System",
  "Wisconsin Retirement System",
  "Wyoming Retirement System, Public Employees’ Pension Plan",
  "Maine Public Employees Retirement System (PERS) Defined Benefit Plan",
  "Maryland State Employees’ Retirement System"
  
)


reason.data <- data.table(reason.data)

reason.data[plan_name %in% blended.teach]$type_of_employees_covered <- "Plan covers state, local, and teachers"

#View(reason.data)


View(reason.data %>% 
       filter(type_of_employees_covered == "Plan covers teachers", year == 2019)
     %>% select(year, state, plan_name,type_of_employees_covered)
)
##

##Florida FRS 2017 data
View(reason.data %>% 
       filter(plan_name == "Florida Retirement System", year == 2017)
     %>% select(year, state, plan_name, ava, aal, funded_ratio,type_of_employees_covered)
)

##New Mexico PERA 2017 data
unique(reason.data %>% 
       filter( year == 2017)
     %>% select(type_of_employees_covered)
)
#################