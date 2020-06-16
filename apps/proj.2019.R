####### UAL Shiny App (by Anil Niraula) #######
####### Reason Foundation
#### Data: Reason

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
library(tidyverse)
#library(openxlsx)
library(tseries)
library(ggplot2)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
#library(shinyFiles)
library(DT)
library(plotly)
#devtools::install_github("ropensci/plotly")
library(dplyr)
library(plyr)

# Load Reason pension data (state-level) ---------------------------------------------------------------
urlfile="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/reason.data.state.csv"
reason.data2 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data2 <- data.table(reason.data2 %>% arrange(year, plan_name))

#Filter out variables
reason.data2 <- reason.data2 %>%
  select(
    year,
    plan_name = display_name,
    state,
    return_1yr = x1_year_investment_return_percentage,
    actuarial_cost_method_in_gasb_reporting,
    funded_ratio = actuarial_funded_ratio_percentage,
    actuarial_valuation_date_for_gasb_schedules,
    actuarial_valuation_report_date,
    ava = actuarial_value_of_assets_gasb_dollar,
    mva = market_value_of_assets_dollar,
    mva_smooth = market_assets_reported_for_asset_smoothing,#added
    aal = actuarially_accrued_liabilities_dollar,
    tpl = total_pension_liability_dollar,
    adec = actuarially_required_contribution_dollar,
    adec_paid_pct = actuarially_required_contribution_paid_percentage,
    amortizaton_method,
    asset_valuation_method_for_gasb_reporting,
    total_benefit_payments = total_benefits_paid_dollar,#added
    benefit_payments = benefit_payments_dollar,
    refunds = refunds_dollar,#added
    admin_exp = administrative_expense_dollar,
    cost_structure,
    payroll = covered_payroll_dollar,
    ee_contribution = employee_contribution_dollar,
    ee_nc_pct = employee_normal_cost_percentage,
    er_contribution = employer_contribution_regular_dollar,
    er_nc_pct = employer_normal_cost_percentage,
    er_state_contribution = employer_state_contribution_dollar,
    er_proj_adec_pct = employers_projected_actuarial_required_contribution_percentage_of_payroll,
    other_contribution = other_contribution_dollar,#added
    other_additions = other_additions_dollar,#added
    fy = fiscal_year,
    fy_contribution = fiscal_year_of_contribution,
    inflation_assum = inflation_rate_assumption_for_gasb_reporting,
    arr = investment_return_assumption_for_gasb_reporting,
    number_of_years_remaining_on_amortization_schedule,
    payroll_growth_assumption,
    total_amortization_payment_pct = total_amortization_payment_percentage,
    total_contribution = total_contribution_dollar,
    total_nc_pct = total_normal_cost_percentage,
    total_number_of_members,
    total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
    type_of_employees_covered,
    unfunded_actuarially_accrued_liabilities_dollar,
    wage_inflation)

reason.data2 <- reason.data2 %>%
  select(
    year, plan_name, state, return_1yr,
    aal, mva, arr, payroll, payroll_growth_assumption, total_nc_pct,
    total_proj_adec_pct, total_benefit_payments)

reason.data2$total_proj_adec_pct <- as.numeric(reason.data2$total_proj_adec_pct)
reason.data2$aal <- as.numeric(reason.data2$aal)
reason.data2$mva <- as.numeric(reason.data2$mva)
reason.data2$total_nc_pct <- as.numeric(reason.data2$total_nc_pct)
reason.data2$payroll <- as.numeric(reason.data2$payroll)
reason.data2$payroll_growth_assumption <- as.numeric(reason.data2$payroll_growth_assumption)
reason.data2$arr <- as.numeric(reason.data2$arr)
reason.data2$return_1yr <- as.numeric(reason.data2$return_1yr)
reason.data2$total_benefit_payments <- as.numeric(reason.data2$total_benefit_payments)

#Save 2019 average returnf or later
return2019 <- mean(reason.data2[year == 2019 & !is.na(return_1yr)]$return_1yr)

#Adding Arizona EORP plan
urlfile2="https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/Arizona%20EORP.csv"
ArizonaEORP <-data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
ArizonaEORP <- ArizonaEORP[!20:25,]
#head(ArizonaEORP)
ArizonaEORP <- data.table(ArizonaEORP %>%
                            select(
                              year = fy, plan_name = PlanName, state = StateName, return_1yr = InvestmentReturn_1yr,
                              aal = ActLiabilities_GASB, mva = MktAssets_net, arr = InvestmentReturnAssumption_GASB, 
                              payroll = payroll, payroll_growth_assumption = PayrollGrowthAssumption, total_nc_pct=NormCostRate_tot,
                              total_proj_adec_pct=ReqContRate_tot,  total_benefit_payments = expense_TotBenefits))
#View(ArizonaEORP)
reason.data2 <- data.table(rbind(reason.data2, ArizonaEORP, fill=TRUE))
#Fix CalPERS ARR
reason.data2[plan_name == "CalPERS - California Public Employees Retirement Fund (CalPERS)*" & year > 2016]$arr <-c(0.0715, 0.0715, 0.07)
#colnames(reason.data2)
reason.data1.n19 <- data.table(reason.data2[year == 2019])
reason.data1.n19$year <- 2019

#Save plan names that miss aal or mva for 2019
names <- reason.data1.n19[is.na(mva) | is.na(aal)]$plan_name
#reason.data1.n20[plan_name == names[2]]$aal
reason.data2[year == 2018 & is.na(payroll_growth_assumption)]$payroll_growth_assumption <- 0.03

###2019 projections
for (i in 1:length(names)){
  reason.data1.n19[plan_name == names[i]]$aal <-  
    (gl(reason.data2[plan_name == names[i]]$aal) *(1 + gl(reason.data2[plan_name == names[i]]$arr)) 
     + (gl(reason.data2[plan_name == names[i]]$total_nc_pct)*gl(reason.data2[plan_name == names[i]]$payroll) +
          gl(reason.data2[plan_name == names[i]]$total_benefit_payments))*
       (1 + gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption))*
       (1 + gl(reason.data2[plan_name == names[i]]$arr)) ^ 0.5)
  reason.data1.n19[plan_name == names[i]]$mva <- 
    (gl(reason.data2[plan_name == names[i]]$mva)*(1 + return2019/100) 
     + (gl(reason.data2[plan_name == names[i]]$total_proj_adec_pct)*gl(reason.data2[plan_name == names[i]]$payroll) +
          gl(reason.data2[plan_name == names[i]]$total_benefit_payments))*
       (1 + gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption)) * (1 + return2019/100) ^ 0.5)
  
  reason.data1.n19[plan_name == names[i]]$payroll <- 
    (gl(reason.data2[plan_name == names[i]]$payroll)*
       (1 + gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption)))
  
  reason.data1.n19[plan_name == names[i]]$total_benefit_payments <- 
    (gl(reason.data2[plan_name == names[i]]$total_benefit_payments)*
       (1 + gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption)))
  
  #reason.data1.n19.na[plan_name == reason.data2[i]]$year <- c(2019
  reason.data1.n19[plan_name == names[i]]$plan_name <- c(gl(reason.data2[plan_name == names[i]]$plan_name))
  reason.data1.n19[plan_name == names[i]]$state <- c(gl(reason.data2[plan_name == names[i]]$state))
  reason.data1.n19[plan_name == names[i]]$arr <- c(gl(reason.data2[plan_name == names[i]]$arr))
  reason.data1.n19[plan_name == names[i]]$return_1yr <- return2019/100
  reason.data1.n19[plan_name == names[i]]$total_nc_pct <- 
    (gl(reason.data2[plan_name == names[i]]$total_nc_pct))
  reason.data1.n19[plan_name == names[i]]$total_proj_adec_pct <- 
    (gl(reason.data2[plan_name == names[i]]$total_proj_adec_pct))
  reason.data1.n19[plan_name == names[i]]$payroll_growth_assumption <- 
    (gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption))
}

#Truncate datat for 2001-18 period
reason.data2 <- reason.data2[year < 2019]
#Add data with 2019 projections
reason.data2 <- data.table(bind_rows(reason.data2, reason.data1.n19) %>% arrange(plan_name))
#Assign paroll growth assumption
table(is.na(reason.data2[year == 2019]$mva))#any more missing values for 2019?
#write.xlsx(PPD2.3, file = "/Users/anilniraula/Downloads/PPD_Avgpayroll.xlsx")
reason.data2 <- data.table(reason.data2)

#Update plan names with abbreviations
urlfile2="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/Reason_State_Names_Mod.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(plan.names)
for (i in 1:plan.names[,.N]){
  reason.data2[plan_name %in% plan.names[i,1]]$plan_name <- as.character(plan.names[i,2])
}
#Save end datat with 2019 projections
#write.csv(reason.data2, file = "/Users/anilniraula/Downloads/reason.data.2019.csv")
