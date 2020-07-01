####### Reason Database in R
##### by Anil Niraula #####
####### Reason Foundation
#### Data: Reason

rm(list=ls())
###Load/install packages
library(reasontheme)
library(pensionviewr)
library(ggplot2)
library(tidyverse)
#library(janitor)
#library(openxlsx)
library(tseries)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(base64enc)
#devtools::install_github("ropensci/plotly")
library(dplyr)
library(plyr)

pl <- data.table(planList())
#Custom function to pull + filter data from teh database
filteredData <- function(data, y, fy){
  Plan <- pullData(data, y)
  Plan <- Plan %>%
    filter(year >= fy)
  Plan <- Plan %>%
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
      uaal = unfunded_actuarially_accrued_liabilities_dollar,
      wage_inflation
      )
}

plans <- c(pl$display_name)
#View(plans)

##Pull data using a custom function
Plan <- filteredData(pl, "Idaho Public Employee Retirement System", 1998)
#######################
##########
