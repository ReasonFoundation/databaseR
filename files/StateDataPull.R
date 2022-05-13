### Reason Database Viewer ###
#(by Anil Niraula)#


rm(list=ls())

###Load/install packagesData
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")
#install.packages('devtools')
# library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
# devtools::install_github(
#   "ANiraula/pensionviewr_private",
#   ref = "master", auth_token = "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",
#   force = TRUE
# )

# #devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(tidyverse)
library(plyr)
#library(openxlsx)
library(tseries)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
library(shinymanager)
library(repmis)
#library(shinyFiles)
library(DT)
library(plotly)
library(grid)
library(readr)
#Shiny
library(rlang)
library(purrr)
library(rpart)
library(vip)
library(skimr)
library(renv)
library(profvis)

pl <- planList()
## Pull all state-level data
#reason.data <- pullData(pl, pl$display_name)

### Modified pullStateData() function ---------------------
pullStateData <- function (FY)
{
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  query <- paste("select * from pull_data_state_only()\nwhere year > '",
                 paste(FY - 1), "'\nand attribute_name in ('1 Year Investment Return Percentage',
                 \n'PPD ID',
                 \n'Number of Employers',
                 \n'Number of Plans',\n'Actuarial Cost Method in GASB Reporting',
                 \n'Employer Normal Cost Percentage',
                 \n'Total Normal Cost Dollar',\n'Actuarial Funded Ratio Percentage',
                 \n'Market Investment Return (MVA Basis)',
                 \n'Actuarial Value of Assets GASB Dollar',
                 \n'Market Value of Assets Dollar',
                 \n'Market Assets Reported for Asset Smoothing',
                 \n'Actuarially Accrued Liabilities Dollar',
                 \n'Total Pension Liability Dollar',
                 \n'Actuarially Determined Contribution Percentage of Payroll',
                 \n'Actuarially Required Contribution Dollar',
                 \n'Actuarially Required Contribution Paid Percentage',
                 \n'Total Employer Contributions',
                 \n'Statutory Payment Dollar',
                 \n'Statutory Payment Percentage',
                 \n'Amortizaton Method',
                 \n'Total Benefits Paid Dollar',
                 \n'Benefit Payments Dollar',
                 \n'Refunds Dollar',
                 \n'Administrative Expense Dollar',
                 \n'Cost Structure',
                 \n'Asset Valuation Method for GASB Reporting',
                 \n'Covered Payroll Dollar',
                 \n'Employee Contribution Dollar',
                 \n'Employee Normal Cost Percentage',
                 \n'Employer Contribution Regular Dollar',
                 \n'Employer Normal Cost Dollar',
                 \n'Employer State Contribution Dollar',
                 \n'ADEC as a Percent of Payroll',
                 \n'Other Contribution Dollar',
                 \n'Other Additions',
                 \n'Fiscal Year of Contribution',
                 \n'Inflation Rate Assumption For GASB Reporting',
                 \n'Investment Return Assumption for GASB Reporting',
                 \n'Discount Rate Assumption',
                 \n'Number of Years Remaining on Amortization Schedule',
                 \n'Payroll Growth Assumption',
                 \n'Unfunded Actuarially Accrued Liabilities Dollar',
                 \n'Total Contribution Dollar',
                 \n'Total Normal Cost Percentage',
                 \n'Net Pension Liability Dollar',
                 \n'Net Pension Liability Assuming 1% Decrease in Discount Rate',
                 \n'Net Pension Liability Assuming 1% Increase in Discount Rate',
                 \n'Total Number Of Members',
                 \n'Total Projected Actuarial Required Contribution Percentage Of Payroll',
                 \n'Actuarially Required Contribution Paid Dollar',
                 \n'Actuarially Determined Contribution Paid Dollar',
                 \n'Type of Employees Covered',
                 \n'Total Amortization Payment Percentage',
                 \n'Wage Inflation')")
  result <- RPostgres::dbSendQuery(con, query)
  all_data <- RPostgres::dbFetch(result) %>% janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  all_data %>% dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>% dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value) %>%
    dplyr::select(-.data$row_id) %>% 
    dplyr::arrange(display_name, year) %>%
    janitor::clean_names()
}


### Modified filterData() function ---------------------

filterData2 <- function(Data, fy = 2001,
                        inflation = TRUE,
                        expenditure = TRUE,
                        employee = NULL,
                        sp500 = TRUE,
                        dcData = TRUE,
                        treasury = TRUE,
                        blend.teacher = FALSE,
                        source = FALSE) {
  if (isTRUE(blend.teacher)) {
    blended.teach <- c("Arizona State Retirement System",
                       "Delaware State Employees’ Pension Plan", "District of Columbia Teachers Retirement Fund",
                       "Florida Retirement System", "Employee Retirement System of Hawaii",
                       "Idaho Public Employee Retirement System", "Iowa Public Employees' Retirement System",
                       "Kansas Public Employees' Retirement System", "Public Employees' Retirement System of Mississippi",
                       "Nevada Public Employees Retirement System", "New Hampshire Retirement System",
                       "North Carolina Teachers' and State Employees' Retirement System",
                       "Oregon Public Employees Retirement System", "Rhode Island Employees Retirement System",
                       "South Carolina Retirement Systems", "South Dakota Retirement System",
                       "Tennessee Consolidated Retirement System, Teachers Pension Plan",
                       "Utah Retirement Systems, Noncontributory Retirement System",
                       "Virginia Retirement System", "Wisconsin Retirement System",
                       "Wyoming Retirement System, Public Employees’ Pension Plan",
                       "Maine Public Employees Retirement System (PERS) Defined Benefit Plan",
                       "Maryland State Employees’ Retirement System")
    Data <- data.frame(Data)
    Data <- Data %>% mutate(type_of_employees_covered = case_when(display_name %in%
                                                                    blended.teach ~ "Plan covers state, local and teachers",
                                                                  TRUE ~ type_of_employees_covered))
  }
  Data <- data.frame(Data)
  columns <- c("total_pension_liability_dollar", "wage_inflation",
               "payroll_growth_assumption", "other_contribution_dollar",
               "other_additions_dollar", "x1_year_investment_return_percentage",
               "amortizaton_method", "number_of_years_remaining_on_amortization_schedule",
               "amortization_payment_total_amount",
               "x1_year_investment_return_percentage",
               "total_normal_cost_dollar", "fiscal_year_of_contribution",
               "statutory_payment_dollar", "statutory_payment_percentage",
               "ppd_fund_id", "number_of_employers", "number_of_plans", "ppd_id",
               "discount_rate_assumption", "market_investment_return_mva_basis", 
               "actuarially_determined_contribution_dollar",
               "net_pension_liability_dollar",
               "administrative_expense_dollar",
               "employer_state_contribution_dollar",
               "total_amortization_payment_percentage",
               "type_of_employees_covered",
               "total_contribution_dollar",
               "employer_contribution_regular_dollar",
               "actuarial_cost_method_in_gasb_reporting",
               "net_pension_liability_assuming_1_percent_decrease_in_discount_rate",
               "net_pension_liability_assuming_1_percent_increase_in_discount_rate",
               "actuarially_required_contribution_paid_dollar",
               "actuarially_required_contribution_paid_percentage",
               "total_benefits_paid_dollar",
               "benefit_payments_dollar",
               "refunds_dollar",
               "actuarially_determined_contribution_paid_dollar",
               "cost_structure", "employer_normal_cost_percentage", "actuarial_value_of_assets_dollar",
               "asset_valuation_method_for_gasb_reporting", "inflation_rate_assumption",
               "total_number_of_members", "total_projected_actuarial_required_contribution_percentage_of_payroll",
               "market_assets_reported_for_asset_smoothing")
  columns2 <- c("test")
  for (i in (1:length(columns))) {
    if (sum(colnames(Data) %in% columns[i]) == 0) {
      columns2 <- rbind(columns2, columns[i])
    }
  }
  columns2 <- as.character(columns2 <- columns2[2:length(columns2)])
  cols <- matrix(NA, length(Data[, 1]), length(columns2))
  colnames(cols) <- columns2
  Data <- cbind(Data, cols)
  Data <- data.frame(Data)
  Data <- Data %>% arrange(state, display_name, year)
  Data$year <- as.numeric(Data$year)
  if (isTRUE(inflation)) {
    library(readxl)
    library(httr)
    url1 <- "https://github.com/ReasonFoundation/databaseR/raw/master/files/CPI_by_Region_DOL.xlsx"
    GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
    cpi <- read_excel(tf, sheet = 1)
    regions <- read_excel(tf, sheet = 2)
    cpi <- data.table(cpi)
    regions <- data.table(regions)
    cpi$cpi_change <- NA
    for (i in (2:length(cpi$region))) {
      cpi$cpi_change[i] <- (cpi$cpi[i]/cpi$cpi[i - 1]) -
        1
    }
    Data <- Data %>% left_join(regions, by = "state")
    Data <- Data %>% left_join(cpi, by = c("year", "region"))
    Data <- Data %>% select(-cpi)
  }
  
  if (isTRUE(sp500)) {
    urlfile2 = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/S%26P500.csv"
    
    
    SP500 <- data.table(
      read_csv(url(urlfile2), col_names = TRUE, na = c(""), 
               skip_empty_rows = TRUE, col_types = NULL))
    
    Data <- Data %>% left_join(SP500, by = c("year"))
    
  }
  
  if (isTRUE(dcData)) {
    
    #Data <- data.table(pullStateData(2001))
    
    urlfile3 = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/DCPlanData.csv"
    
    DCPlanData <- data.table(
      read_csv(url(urlfile3), col_names = TRUE, na = c(""), 
               skip_empty_rows = TRUE, col_types = NULL))
    
    DCPlanData <- DCPlanData %>% 
      select(year = fy, ppd_id, plantype,
             ee_contrate_mandatory, er_contrate_fixedrate, 
             planname, primary,
             eligible_eegroups, assets, members_total, actives, inactives, coveredpayroll) %>% 
      replace(is.na(.), 0)
    
    DCPlanData$ppd_id <- as.numeric(DCPlanData$ppd_id)
    DCPlanData$fy <- as.numeric(DCPlanData$fy)
    DCPlanData$er_contrate_fixedrate <- as.numeric(DCPlanData$er_contrate_fixedrate)
    DCPlanData$ee_contrate_mandatory <- as.numeric(DCPlanData$ee_contrate_mandatory)
    DCPlanData$assets <- as.numeric(DCPlanData$assets)
    DCPlanData$members_total <- as.numeric(DCPlanData$members_total)
    DCPlanData$actives <- as.numeric(DCPlanData$actives)
    DCPlanData$inactives <- as.numeric(DCPlanData$inactives)
    
    for(i in (1:length(DCPlanData$members_total))){
      
      DCPlanData$members_total[i] <- max(DCPlanData$members_total[i], (DCPlanData$actives[i]+DCPlanData$inactives[i]))
      
    }
    
    Data$ppd_id <- as.numeric(Data$ppd_id)
    Data$year<- as.numeric(Data$year)
    
    Data <- Data %>% left_join(DCPlanData, by = c("ppd_id", "year"))
    
  }
  if (isTRUE(treasury)) {
    urlfile <- "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/treasury.csv"
    treasury <- data.table(
      read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
    
    colnames(treasury)[2] <- "treasury_30"
    Data <- Data %>% left_join(treasury, by = c("year"))
    
  }
  
  if (isTRUE(expenditure)) {
    
    #1. General Fund Exp./Rev. NASBO (State)
    #2. Total Exp./Rev. NASBO (State) --> general funds, other state funds, bonds, and federal funds.
    #3. Total Exp./Rev. Urban (State & Local) --> state and local direct general expenditures (exclude “business-like” activities such as utilities and transfers between state and local governments.)
    
    library(readxl)
    library(httr)
    urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/NASBO_Exp_Report_Data_v1.csv"
    NASBO<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
    NASBO <- setDT(NASBO)
    NASBO <- NASBO %>% select(YEAR, STATE,"TOTAL_CAPI") %>% filter(YEAR>2000)
    #View(NASBO)
    colnames(NASBO)[1] <- "year"
    colnames(NASBO)[2] <- "state"
    Data <- Data %>% left_join(NASBO, by = c("year","state"))
    ####
    urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/UrbanCensus_ExpRev_2000-2018.csv"
    Urban<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
    Urban <- setDT(Urban)
    
    Urban <- Urban %>% select(Year, State,"(R01) Total Revenue", "(E001) Total Expenditure")
    
    colnames(Urban)[1] <- "year"
    colnames(Urban)[2] <- "state"
    
    Data <- Data %>% left_join(Urban, by = c("year","state"))
    
  }
  
  Data <- Data %>% select(year, plan_name = display_name,
                          state, if (isTRUE(source)) {
                            "data_source_name"
                          }, return_1yr = x1_year_investment_return_percentage,
                          ava_return = market_investment_return_mva_basis, actuarial_cost_method_in_gasb_reporting,
                          funded_ratio = actuarial_funded_ratio_percentage, ava = actuarial_value_of_assets_dollar, #actuarial_value_of_assets_gasb_dollar (PPD)
                          mva = market_value_of_assets_dollar, #mva_smooth = market_assets_reported_for_asset_smoothing,
                          aal = actuarially_accrued_liabilities_dollar,
                          adec1 = actuarially_determined_contribution_dollar,
                          adec2 =   actuarially_required_contribution_paid_dollar,  #actuarially_required_contribution_dollar (PPD)
                          adec_paid_pct = actuarially_required_contribution_paid_percentage,
                          adec_pct = adec_as_a_percent_of_payroll, #instead of employers_projected_actuarial_required_contribution
                          arc_paid_dollar = actuarially_required_contribution_paid_dollar, #NEW
                          adec_paid_dollar = actuarially_determined_contribution_paid_dollar,#NEW
                          statutory = statutory_payment_dollar, statutory_pct = statutory_payment_percentage,
                          amortizaton_method, total_benefit_payments = total_benefits_paid_dollar,
                          benefit_payments = benefit_payments_dollar, refunds = refunds_dollar,
                          admin_exp = administrative_expense_dollar, cost_structure,
                          state_exp_NASBO = TOTAL_CAPI, #gf_rev_NASBO = "TOTAL REV",
                          state.local_exp_Census = "(R01) Total Revenue", state.local_rev_Census = "(E001) Total Expenditure",
                          fund_id = ppd_fund_id, ppd_id = ppd_id, employers = number_of_employers,
                          plans = number_of_plans, asset_valuation_method_for_gasb_reporting,
                          dc_plantype = plantype,  dc_er_pct = er_contrate_fixedrate, #dc_eligble = eligible_eegroups,
                          dc_ee_pct = ee_contrate_mandatory, dc_plan = planname, dc_primary = primary,
                          dc_assets = assets, dc_members = members_total, dc_payroll = coveredpayroll,
                          payroll = covered_payroll_dollar, ee_contribution = employee_contribution_dollar,
                          ee_nc_pct = employee_normal_cost_percentage,
                          er_nc_pct = employer_normal_cost_percentage,total_nc_pct = total_normal_cost_percentage, 
                          er_contribution = employer_contribution_regular_dollar, er_state_contribution = employer_state_contribution_dollar,
                          other_contribution = other_contribution_dollar, other_additions = other_additions_dollar,
                          fy_contribution = fiscal_year_of_contribution, inflation_assum = inflation_rate_assumption,
                          arr = investment_return_assumption_for_gasb_reporting,
                          dr = discount_rate_assumption, number_of_years_remaining_on_amortization_schedule,
                          payroll_growth_assumption, total_amortization_payment_pct = total_amortization_payment_percentage,
                          total_amo_payment = amortization_payment_total_amount, #Added
                          total_contribution = total_contribution_dollar,
                          total_nc_dollar = total_normal_cost_dollar, total_number_of_members,
                          total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
                          tpl = total_pension_liability_dollar,
                          npl_pct_down = net_pension_liability_assuming_1_percent_decrease_in_discount_rate, #Added
                          npl = net_pension_liability_dollar, #Added
                          npl_pct_up = net_pension_liability_assuming_1_percent_increase_in_discount_rate, #Added
                          type_of_employees_covered, unfunded_actuarially_accrued_liabilities_dollar,
                          wage_inflation, region, cpi_change, sp_500, treasury_30)
  
  
  ## Combining ARC & ADEC
  Data$state.local_exp_Census <- as.numeric(Data$state.local_exp_Census)
  Data$state.local_rev_Census <- as.numeric(Data$state.local_exp_Census)
  Data$state_exp_NASBO <- as.numeric(Data$state_exp_NASBO)
  Data$dc_primary <- ifelse(Data$dc_primary == 1, "Yes", "No")
  
  Data$adec1 <- as.numeric(Data$adec1)
  Data$adec2 <- as.numeric(Data$adec2)
  Data$adec_paid_dollar <- as.numeric(Data$adec_paid_dollar)
  Data$arc_paid_dollar <- as.numeric(Data$arc_paid_dollar)
  Data$arc_adec_dollar <- ifelse(is.na(Data$adec1) == TRUE,Data$adec2, Data$adec1)
  Data$arc_adec_paid_dollar <- ifelse(is.na(Data$arc_paid_dollar) == TRUE,
                                      ifelse(is.na(Data$statutory_payment_dollar) == TRUE,statutory_payment_dollar, 
                                             Data$adec_paid_dollar), 
                                      Data$arc_paid_dollar)
  
  Data$fy_contribution <- as.numeric(Data$fy_contribution)
  Data$year <- as.numeric(Data$year)
  Data$fy_contribution <- round(Data$fy_contribution, 0)
  Data <- Data %>% filter(year >= fy) #%>% select(-adec1, -adec2)
  
  #https://www.urban.org/policy-centers/cross-center-initiatives/state-and-local-finance-initiative/projects/state-fiscal-briefs/florida
  
  if (isTRUE(expenditure)) {
    Data$ava <- as.numeric(Data$ava)
    Data$aal <- as.numeric(Data$aal)
    Data$state_exp_NASBO <- as.numeric(Data$state_exp_NASBO)
    Data$state.local_exp_Census <- as.numeric(Data$state.local_exp_Census)
    Data <- Data %>% group_by(state, year) %>% mutate(ual_exp_NASBO = round((sum(aal)-sum(ava))/(state_exp_NASBO*1000000),2),
                                                      ual_exp_Census = round((sum(aal)-sum(ava))/(state.local_exp_Census*1000),2))
  }
  
  if (is_null(employee)) {
    employee <- employee
  }
  else if (employee == "teacher") {
    employee <- c("Plan covers teachers")
  }
  else if (employee == "state and local") {
    employee <- c("Plan covers state and local employees")
  }
  else if (employee == "police and fire") {
    employee <- c("Plan covers police and/or fire")
  }
  else if (employee == "state") {
    employee <- c("Plan covers state employees")
  }
  else if (employee == "local") {
    employee <- c("Plan covers local employees")
  }
  else if (employee == "state, local, and teachers") {
    employee <- c("Plan covers state, local and teachers")
  }
  if (is_null(employee)) {
    Data <- data.frame(Data) %>% replace(is.na(.), 0)
  }
  else {
    Data %>% filter(type_of_employees_covered == paste(employee)) %>% replace(is.na(.), 0)
  }
}

######### Pulling and modifying state data
#------------------------------------------------------------------------------------------------------------------------------
##Pull State data---------------------
reason.data <- pullStateData(2001)

## Alternative pulling option (raw state + local data)
#reason.data2 <- reason.data %>% filter(administering_government_type == 0)

## Filter columns/data---------------------
reason.data2 <- filterData2(reason.data, 2001)

# write_csv(pl, "pl.csv")
## Keep handful of columns
reason.data2 <- reason.data2 %>% select(year, plan_name, state, 
                                        aal, mva, return_1yr,
                                        dr, arr, payroll_growth_assumption, payroll,
                                        adec_pct,
                                        adec_paid_dollar,
                                        statutory,
                                        statutory_pct,
                                        ee_contribution,
                                        er_contribution,
                                        ee_nc_pct,
                                        er_nc_pct,
                                        total_nc_dollar,
                                        total_nc_pct, 
                                        total_amo_payment,
                                        total_proj_adec_pct,
                                        total_contribution,
                                        total_benefit_payments) %>% filter(year >= 2001)

reason.data2 <- reason.data2 %>% group_by(plan_name) %>% filter(!duplicated(year))
#View(reason.data2)

## Use PPD data to convert text to numeric (e.g., range of ARR)---------------------

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-2022-04-01.csv"
#urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-latest_Q1_2021.csv"
PPD<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
PPD <- setDT(PPD)
PPD <- data.table(PPD[AdministeringGovt == 0] %>%
                    select(fy, PlanName, StateName, ActLiabilities_GASB, MktAssets_net,ActAssets_GASB,
                           ActFundedRatio_GASB,
                           InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
                           PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, PercentReqContPaid,
                           expense_TotBenefits,contrib_ER_regular) %>% arrange(PlanName))


urlfile3="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/pl.csv"			
pl <- data.table(read_csv(url(urlfile3), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))	


PPD$InvestmentReturn_1yr <- as.numeric(PPD$InvestmentReturn_1yr)
reason.data2 <- data.table(reason.data2)
reason.data2[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$arr <-
  PPD[PlanName == "Michigan Public Schools" & fy > 2010 & fy < 2021]$InvestmentReturnAssumption_GASB

#View(reason.data2[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$year)
#View(PPD[PlanName == "Michigan Public Schools" & fy > 2010 & fy < 2021]$InvestmentReturnAssumption_GASB)
#### Pull Return + DR data from PPD (When 2 Plans)
reason.data2[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$return_1yr <-
  PPD[PlanName == "Michigan Public Schools" & fy > 2010 & fy < 2021]$InvestmentReturn_1yr

reason.data2[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$dr <- 
  reason.data2[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$arr

#####
reason.data2[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$return_1yr <-
  PPD[PlanName == "Minnesota Teachers" & fy > 2003 & fy < 2021]$InvestmentReturn_1yr

reason.data2[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$arr <-
  PPD[PlanName == "Minnesota Teachers" & fy > 2003 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data2[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$dr <- 
  reason.data2[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$arr

#################
#View(PPD[PlanName == "Delaware State Employees"] %>% select(fy,ActLiabilities_GASB))
#View(reason.data2[plan_name == "Delaware State Employees’ Pension Plan"] %>% select(year,aal))

#####
reason.data2[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$return_1yr <-
  PPD[PlanName == "Kentucky ERS" & fy > 2000 & fy < 2021]$InvestmentReturn_1yr

reason.data2[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$arr <-
  PPD[PlanName == "Kentucky ERS" & fy > 2000 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data2[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$dr <- 
  reason.data2[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$arr

#################
reason.data2[plan_name == "New Hampshire Retirement System" & year > 2003 & year < 2021]$arr <- 
  PPD[PlanName == "New Hampshire RS" & fy > 2003 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data2[plan_name == "New Hampshire Retirement System"]$dr <- 
  reason.data2[plan_name == "New Hampshire Retirement System"]$arr
####

reason.data2[plan_name == "New Mexico Public Employees Retirement Association"& year > 2003 & year < 2021]$dr <- 
  PPD[PlanName == "New Mexico PERA"& fy > 2003 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data2[plan_name == "New Mexico Public Employees Retirement Association"& year > 2003 & year < 2021]$arr <- 
  reason.data2[plan_name == "New Mexico Public Employees Retirement Association"& year > 2003 & year < 2021]$dr
#################---------------------

reason.data2 <- data.table(reason.data2)
reason.data2$ava <- as.numeric(reason.data2$ava)
reason.data2$aal <- as.numeric(reason.data2$aal)
reason.data2$mva <- as.numeric(reason.data2$mva)
reason.data2$funded_ratio <- as.numeric(reason.data2$funded_ratio)

## Add EORP plan ---------------------
eorp <- pullData(pl, "Arizona Elected Officials Retirement Plan")
eorp <- filterData2(eorp)
eorp <- eorp %>% select(year, plan_name, state, 
                        aal, mva, return_1yr,
                        dr, arr, payroll_growth_assumption, payroll,
                        adec_pct,
                        adec_paid_dollar,
                        statutory,
                        statutory_pct,
                        ee_contribution,
                        er_contribution,
                        ee_nc_pct,
                        er_nc_pct,
                        total_nc_dollar,
                        total_nc_pct, 
                        total_amo_payment,
                        total_proj_adec_pct,
                        total_contribution,
                        total_benefit_payments) %>% filter(year >= 2001)
eorp <- eorp %>% filter(!duplicated(year))

## Combine ---------------------
reason.data2 <- reason.data2 %>% select(-ava, -funded_ratio) %>% arrange(state, plan_name, year)
reason.data2 <- rbind(reason.data2, eorp)

#View(reason.data2 %>% filter(year == 2019))
#write.xlsx(reason.data2, "StateDataFiltered_2001-2021.xlsx")
#View(reason.data2 %>% filter(state == "Arizona Elected Officials"))
