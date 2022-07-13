### Reason Database Viewer ###
#(by Anil Niraula)#

rm(list=ls())

###Load/install packages
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")

######## Need "devtools" pkg to download our "pensionviewr" R package

# install.packages('devtools')
# library(devtools)

 # devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
 # devtools::install_github(
 #   "ANiraula/pensionviewr_private",
 #   ref = "master", auth_token = "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",
 #   force = TRUE
 # )

########

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
############# PullData #############

pullData <- function (pl, plan_name = "Texas Employees Retirement System") 
{
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  query <- "select plan_annual_master_attribute.year,\n  plan_annual_master_attribute.plan_id,\n  plan.display_name, \n plan.type_of_employee_covered, \n  state.name as state,\n  plan_master_attribute_names.name as attribute_name,\n  plan_annual_master_attribute.attribute_value\n  from plan_annual_master_attribute\n  inner join plan\n  on plan_annual_master_attribute.plan_id = plan.id\n  inner join government\n  on plan.admin_gov_id = government.id\n  inner join state\n  on government.state_id = state.id\n  inner join plan_master_attribute_names\n  on plan_annual_master_attribute.master_attribute_id = plan_master_attribute_names.id\n  where plan_id = $1"
  plan_id <- pl$id[pl$display_name == plan_name]
  result <- RPostgres::dbSendQuery(con, query)
  RPostgres::dbBind(result, list(plan_id))
  all_data <- RPostgres::dbFetch(result) %>% janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  all_data %>% dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>% 
    dplyr::mutate(row_id = 1:dplyr::n()) %>% dplyr::ungroup() %>% 
    tidyr::spread(.data$attribute_name, .data$attribute_value, 
                  convert = TRUE) %>% dplyr::select(-.data$row_id) %>% 
    janitor::clean_names()
}


############# PullStateData #############

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
                 \n'1-Yr Investment Return',
                 \n'PPD ID',
                 \n'Number of Employers',
                 \n'Number of Plans',
                 \n'Actuarial Cost Method in GASB Reporting',
                 \n'Employer Normal Cost Percentage',
                 \n'Total Normal Cost Dollar',
                 \n'Actuarial Funded Ratio Percentage',
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


#View(colnames(pullStateData(2018)))
#View(pullData(pl, "Arkansas Public Employees System"))

#pl_unique <- unique(pl$display_name)
#Source_Data <- pullSourceData(pl, pl_unique[1], 2001)
#for (i in (2:length(pl_unique))){
#  Source_Data <- rbind.fill(Source_Data, pullSourceData(pl, pl_unique[i], 2001))
#  
#}

#Source_Data <- pullSourceData(pl, pl$display_name, 2001)
#View(Source_Data)
#View(colnames(pullStateData(2001)))
states <- as.character(unique(pl[,3]))
plans <- as.character(unique(pl[,2]))
#View(colnames(pullData(pl[state=="New Mexico"], pl[state=="New Mexico"]$display_name)))
#View(pl)

#x <- pullStateData(2001)
#View(colnames(x))


########## Update w/ latest PPD file 
#https://publicplansdata.org/public-plans-database/download-full-data-set/
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-Q2-2022.csv"
#urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-latest_Q1_2021.csv"
PPD<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
PPD <- setDT(PPD)
#View(colnames(PPD))
#View(PPD %>% select(fy,PlanName,StateName,contrib_ER_tot, contrib_EE_regular, contrib_tot))

#View(PPD$ActAssets_GASB)
#PPD$AdministeringGovt
#Remove Colorado State and Shool plan
#PPD <- PPD[PlanName != "Colorado State and School"]
PPD <- data.table(PPD[AdministeringGovt == 0] %>%
                    select(fy, PlanName, StateName, ActLiabilities_GASB, MktAssets_net,ActAssets_GASB,
                           ActFundedRatio_GASB,
                           InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
                           PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, PercentReqContPaid,
                           expense_TotBenefits,contrib_ER_regular) %>% arrange(PlanName))

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"			
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))			
# pl <- data.table(pl)			
# for (i in 1:plan.names[,.N]){			
#   pl[display_name %in% plan.names[i,1]]$display_name <- as.character(plan.names[i,2])			
# }
# write_csv(pl, "pl.csv")

urlfile3="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/pl.csv"			
pl <- data.table(read_csv(url(urlfile3), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))	

#########
# Filter Downloaded Data -------------------------------------------------------------
#x <- pullData(pl, pl$display_name)
#View(x)
#x2 <- filterData2(x, 2001)
#x3 <- x2 %>% filter(year == 2019, type_of_employees_covered != 0) %>% select(plan_name, state, type_of_employees_covered)
#View(x3)


############# FilterData2 (updated) #############

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
               "payroll_growth_assumption", "payroll_growth_assumption_ppd", "other_contribution_dollar",
               "other_additions_dollar", "x1_year_investment_return_percentage",
               "amortizaton_method", "number_of_years_remaining_on_amortization_schedule",
               "amortization_payment_total_amount",
               "x1_year_investment_return_percentage",
               "x1_year_investment_return_percentage_ppd",
               "total_normal_cost_dollar", "fiscal_year_of_contribution",
               "statutory_payment_dollar", "statutory_payment_percentage",
               "ppd_fund_id", "number_of_employers", "number_of_plans", "ppd_id",
               "discount_rate_assumption", "market_investment_return_mva_basis", 
               "actuarially_determined_contribution_dollar",
               "net_pension_liability_dollar",
               "administrative_expense_dollar",
               "employer_state_contribution_dollar",
               "total_amortization_payment_percentage",
               "investment_return_assumption_for_gasb_reporting",
               "type_of_employees_covered",
               "total_contribution_dollar",
               "total_normal_cost_percentage_ppd",
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
                          }, return_1yr = x1_year_investment_return_percentage,return_1yr_ppd = x1_year_investment_return_percentage_ppd,
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
                          er_nc_pct = employer_normal_cost_percentage,
                          total_nc_pct = total_normal_cost_percentage, 
                          total_nc_pct_ppd = total_normal_cost_percentage_ppd, 
                          er_contribution = employer_contribution_regular_dollar, er_state_contribution = employer_state_contribution_dollar,
                          other_contribution = other_contribution_dollar, other_additions = other_additions_dollar,
                          fy_contribution = fiscal_year_of_contribution, inflation_assum = inflation_rate_assumption,
                          arr = investment_return_assumption_for_gasb_reporting,
                          dr = discount_rate_assumption, number_of_years_remaining_on_amortization_schedule,
                          payroll_growth_assumption,payroll_growth_assumption_ppd, total_amortization_payment_pct = total_amortization_payment_percentage,
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

##Ensure all variables are numeric
reason.data <- pullStateData(2001)
# reason.data <- filterData2(reason.data,
#                            fy = 2001,
#                            inflation = FALSE,
#                            expenditure = TRUE,
#                            employee = NULL,
#                            sp500 = TRUE,
#                            dcData = TRUE,
#                            treasury = TRUE,
#                            blend.teacher = FALSE,
#                             source = FALSE)
# 
# View(reason.data %>% select(year, state, cpi_change,sp_500, dc_er_pct) %>%
#        filter(year == 2018))

################
PPD_ID <- reason.data
PPD_ID <- PPD_ID %>% select(ppd_id, plan_id) 
PPD_ID <- PPD_ID %>% filter(!duplicated(ppd_id))
#View(PPD_ID)
#View(reason.data)
#full.state.data <- reason.data 
reason.data <- filterData2(reason.data, 2001)
colnames(reason.data)
#reason.data <- reason.data %>% filter(!duplicated(year))
#
#View(reason.data %>% filter(year == 2021))
#View(reason.data %>% select(year,arc_paid_dollar, adec_paid_dollar, arc_adec_paid_dollar))
reason.data <- data.table(reason.data)
reason.data$year <- as.numeric(reason.data$year)
reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)
reason.data$mva <- as.numeric(reason.data$mva)
reason.data$return_yr <- as.numeric(reason.data$return_1yr)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$arr <- as.numeric(reason.data$arr)
reason.data$payroll <- as.numeric(reason.data$payroll)
reason.data$payroll_growth_assumption <- as.numeric(reason.data$payroll_growth_assumption)
reason.data$total_nc_pct <- as.numeric(reason.data$total_nc_pct)
reason.data$benefit_payments <- as.numeric(reason.data$benefit_payments)
#View(Data %>% filter(plan_name == "Idaho Public Employee Retirement System"))

##Filter out reason data for variables we commonly use for pension analysis
#reason.data <- pullStateData(2001)
#View(reason.data %>% filter(display_name == ""))
#reason.data <- filterData(reason.data, 2001)
#reason.data.2020 <- reason.data %>% filter(year == 2020)
#View(reason.data)
#reason.data.2020$ava <- as.numeric(reason.data.2020$ava)
#reason.data.2020$aal <- as.numeric(reason.data.2020$aal)
#sum(na.omit(reason.data.2020$ava))/sum(na.omit(reason.data.2020$aal))
#View(reason.data)

reason.data <- as.data.table(reason.data)
reason.data$year <- as.numeric(reason.data$year)
reason.data$return_1yr <- as.numeric(reason.data$return_1yr)
reason.data$arr <- as.numeric(reason.data$arr)
reason.data$dr <- as.numeric(reason.data$dr)
#

##### Replacing some Reason data with PPD data ####

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-Q2-2022.csv"
#urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-latest_Q1_2021.csv"
PPD<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
PPD <- setDT(PPD)
#View(colnames(PPD))
#View(PPD$ActAssets_GASB)
#PPD$AdministeringGovt
#Remove Colorado State and Shool plan
#PPD <- PPD[PlanName != "Colorado State and School"]
PPD <- data.table(PPD[AdministeringGovt == 0] %>%
                    select(fy, PlanName, StateName, ActLiabilities_GASB, MktAssets_net,ActAssets_GASB,
                           ActFundedRatio_GASB,
                           InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
                           PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, PercentReqContPaid,
                           expense_TotBenefits,contrib_ER_regular) %>% arrange(PlanName))

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"			
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))			
# pl <- data.table(pl)			
# for (i in 1:plan.names[,.N]){			
#   pl[display_name %in% plan.names[i,1]]$display_name <- as.character(plan.names[i,2])			
# }
# write_csv(pl, "pl.csv")

urlfile3="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/pl.csv"			
pl <- data.table(read_csv(url(urlfile3), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))	



PPD$InvestmentReturn_1yr <- as.numeric(PPD$InvestmentReturn_1yr)
reason.data[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$arr <-
  PPD[PlanName == "Michigan Public Schools" & fy > 2010 & fy < 2021]$InvestmentReturnAssumption_GASB

#View(reason.data[display_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$year)
#View(PPD[PlanName == "Michigan Public Schools" & fy > 2010 & fy < 2021]$InvestmentReturnAssumption_GASB)
#### Pull Return + DR data from PPD (When 2 Plans)
reason.data[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$return_1yr <-
  PPD[PlanName == "Michigan Public Schools" & fy > 2010 & fy < 2021]$InvestmentReturn_1yr

reason.data[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$dr <- 
  reason.data[plan_name == "Michigan Public School Employees Retirement System" & year > 2010 & year < 2021]$arr

#####
reason.data[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$return_1yr <-
  PPD[PlanName == "Minnesota Teachers" & fy > 2003 & fy < 2021]$InvestmentReturn_1yr

reason.data[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$arr <-
  PPD[PlanName == "Minnesota Teachers" & fy > 2003 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$dr <- 
  reason.data[plan_name == "Minnesota Teachers Retirement Association" & year > 2003 & year < 2021]$arr

#################
#View(PPD[PlanName == "Delaware State Employees"] %>% select(fy,ActLiabilities_GASB))
#View(reason.data[plan_name == "Delaware State Employees’ Pension Plan"] %>% select(year,aal))

#####
reason.data[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$return_1yr <-
  PPD[PlanName == "Kentucky ERS" & fy > 2000 & fy < 2021]$InvestmentReturn_1yr

reason.data[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$arr <-
  PPD[PlanName == "Kentucky ERS" & fy > 2000 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$dr <- 
  reason.data[plan_name == "Kentucky Employees Retirement System" & year > 2000 & year < 2021]$arr

#################
reason.data[plan_name == "New Hampshire Retirement System" & year > 2003 & year < 2021]$arr <- 
  PPD[PlanName == "New Hampshire RS" & fy > 2003 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data[plan_name == "New Hampshire Retirement System"]$dr <- 
  reason.data[plan_name == "New Hampshire Retirement System"]$arr
####

reason.data[plan_name == "New Mexico Public Employees Retirement Association"& year > 2003 & year < 2021]$dr <- 
  PPD[PlanName == "New Mexico PERA"& fy > 2003 & fy < 2021]$InvestmentReturnAssumption_GASB

reason.data[plan_name == "New Mexico Public Employees Retirement Association"& year > 2003 & year < 2021]$arr <- 
  reason.data[plan_name == "New Mexico Public Employees Retirement Association"& year > 2003 & year < 2021]$dr
#################

###### Normal Cost

reason.data[plan_name == "Wisconsin Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Wisconsin RS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "West Virginia Teachers’ Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "West Virginia Teachers"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "West Virginia Public Employees Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "West Virginia PERS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Virginia Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Virginia RS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Rhode Island Employees Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Rhode Island ERS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Utah Retirement Systems, Noncontributory Retirement System" & year > 2000]$total_nc_pct <-
  PPD[PlanName == "Utah Noncontributory" & fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Iowa Public Employees' Retirement System" & year > 2000]$total_nc_pct <-
  PPD[PlanName == "Iowa PERS" & fy > 2000]$NormCostRate_tot

reason.data[plan_name == "New Hampshire Retirement System" & year > 2000]$total_nc_pct <-
  PPD[PlanName == "New Hampshire RS" & fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Oregon Public Service Retirement Plan" & year > 2000]$total_nc_pct <-
  PPD[PlanName == "Oregon PERS" & fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Pennsylvania Municipal Retirement System" & year > 2000]$total_nc_pct <-
  PPD[PlanName == "Pennsylvania Municipal" & fy > 2000]$NormCostRate_tot

reason.data[plan_name == "North Carolina Local Government Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "North Carolina Local Government"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Nevada Public Employees Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Nevada Regular Employees"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Kentucky Teachers' Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Kentucky Teachers"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Kentucky Employees Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Kentucky ERS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Kentucky County Employees Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Kentucky County"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Kansas Public Employees' Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Kansas PERS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Georgia Employees’ Retirement System"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Georgia ERS"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Connecticut Municipal Employees Retirement Plan"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Connecticut Municipal"& fy > 2000]$NormCostRate_tot

reason.data[plan_name == "Alabama Teachers' Retirement System (TRS)"& year > 2000]$total_nc_pct <-
  PPD[PlanName == "Alabama Teachers"& fy > 2000]$NormCostRate_tot

### Payroll growth assumption

reason.data[plan_name == "West Virginia Teachers’ Retirement System"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "West Virginia Teachers"& fy > 2000]$PayrollGrowthAssumption

reason.data[plan_name == "West Virginia Public Employees Retirement System"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "West Virginia PERS"& fy > 2000]$PayrollGrowthAssumption

reason.data[plan_name == "Rhode Island Employees Retirement System"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "Rhode Island ERS"& fy > 2000]$PayrollGrowthAssumption

reason.data[plan_name == "Nevada Public Employees Retirement System"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "Nevada Regular Employees"& fy > 2000]$PayrollGrowthAssumption

reason.data[plan_name == "Kentucky Employees Retirement System"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "Kentucky ERS"& fy > 2000]$PayrollGrowthAssumption

reason.data[plan_name == "Georgia Employees’ Retirement System"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "Georgia ERS"& fy > 2000]$PayrollGrowthAssumption

reason.data[plan_name == "Connecticut Municipal Employees Retirement Plan"& year > 2000]$payroll_growth_assumption <-
  PPD[PlanName == "Connecticut Municipal"& fy > 2000]$PayrollGrowthAssumption

#########

reason.data <- data.table(reason.data)
reason.data$ava <- as.numeric(reason.data$ava)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$mva <- as.numeric(reason.data$mva)
reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)

#View(reason.data %>% filter(state == "North Carolina" & year == 2020) %>%
#       select(plan_name,ava,mva,aal,funded_ratio) %>% mutate(funded_ratio_mva = sum(mva)/sum(aal),
#                                                   funded_ratio_ava = sum(ava)/sum(aal)))

#View(reason.data %>% filter(year == 2020) %>%
#     select(ava,mva,aal,funded_ratio) %>% 
#       filter(!is.na(ava) | !is.na(aal)) %>% 
#       mutate(funded_ratio_median_ava = sum(ava)/sum(aal)))

#View(reason.data %>% 
#       mutate(calc.funded = ava/aal) %>%
#       filter(round(calc.funded,0) != round(funded_ratio,0)) %>%
#       select(year, plan_name, calc.funded, funded_ratio))


#View(reason.data %>% 
#       mutate(missing.aal = is.na(aal)) %>%
#       group_by(missing.aal) %>%
#       summarize(avg.aal = mean(na.omit(funded_ratio))))

###################
#Using 2 New Functions to download and filter datat from datatabase
#PERSI.data <- pullStateData(2001)
#PERSI.data <- filterData(PERSI.data, 2001)

#pl <- planList()
#filter for PERSI

#PERSI.data$ava <- as.numeric(PERSI.data$ava)
#PERSI.data$aal <- as.numeric(PERSI.data$aal)
#PERSI.data$year <- as.numeric(PERSI.data$year)

#View(PERSI.data)
#%>% filter (plan_name == "Maryland State Employees’ Retirement System")

#urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/StateLocal_UAL.csv"
#total.data <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))


#total.data <- pullData(pl, pl$display_name)
#View(total.data)

#total.data$actuarial_value_of_assets_gasb_dollar <- as.numeric(total.data$actuarial_value_of_assets_gasb_dollar)
#total.data$actuarially_accrued_liabilities_dollar <- as.numeric(total.data$actuarially_accrued_liabilities_dollar)
#total.data$year <- as.numeric(total.data$year)

#total.data2 <- total.data %>% 
#  group_by(year) %>%
#  filter(!is.na(actuarial_value_of_assets_gasb_dollar) & !is.na(actuarially_accrued_liabilities_dollar)) %>%
#  transmute(ava = sum(actuarial_value_of_assets_gasb_dollar),
#            aal = sum(actuarially_accrued_liabilities_dollar)) %>% arrange(year) %>% filter(year > 2000,!duplicated(year))

#View(total.data2)

#urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/StateLocal_UAL.csv"
#total.statelocal <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(total.statelocal)

#reason.data$arr <- as.numeric(reason.data$arr)
#reason.data$aal <- as.numeric(reason.data$aal)
#reason.data$return_1yr <- as.numeric(reason.data$return_1yr)
#reason.data$year <- as.numeric(reason.data$year)
#View(reason.data)
#  x <- pullStateData(2020)
# View(x %>% select(year, display_name,employer_normal_cost_dollar))
# View(x$actuarially_req_contribution_percentage_of_payroll)
###### pullData pull all > 290 columns (PPD & Reason)
###### pullState Data selectively pulls > 45 columns (Mostly Reason)
###### Prioritiazation works if PPD & Reason column names equal?

# x <- pullStateData(2001)
# x <- filterData2(x)
# 
# View(x %>% select(year,
#                   adec,
#                   adec_paid_pct,
#                   adec_pct,
#                   er_contribution))
#                   
# View(x$inflation_assum)
# x <- x %>% select(year, plan_name, ava, aal, unfunded_actuarially_accrued_liabilities_dollar)
# x$mva <- as.numeric(x$mva)
# x$aal <- as.numeric(x$aal)
# x$year <- as.numeric(x$year)
# View(x <- x %>% group_by(year) %>%
#        filter(!is.na(mva) & !is.na(aal) & !duplicated(year)) %>%
#        transmute(ava = sum(mva),
#                  aal = sum(aal),
#                  ual = aal-mva))
# # 
# # x <- x %>% filter(display_name == "r_iana State Employees Retirement System")
# # #View(reason.data)
# # 
# # ################################
# 
# y <- pullData(pl, "Louisiana State Employees Retirement System")
# y <- y %>% filter(!duplicated(year), year > 2000)
# #View(colnames(y))
# #View(y$actuarially_determined_contribution_paid_dollar)
# 
# # # y$adec_paid <- y$actuarially_determined_contribution_dollar - y$actuarially_determined_contribution_missed_dollar
# # # 
#  View(y %>% select(year,
#                   actuarially_required_contribution_paid_dollar,actuarially_determined_contribution_paid_dollar,
#                   payroll_growth_assumption))
#                   ))
#  
# reason.data$return_1yr <- as.numeric(reason.data$return_1yr)
# View(reason.data %>% group_by(year) %>%
#        filter(!is.na(return_1yr)) %>%
#        mutate(return_1yr = as.numeric(return_1yr)) %>%
#        summarise(return = mean(return_1yr)))
# ################################
#View(colnames(y))
#View(x$adec)
#Label state and local plans with*

# urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"
# plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
# #View(plan.names)
# 
# pl <- data.table(pl)
# for (i in 1:plan.names[,.N]){
#   pl[display_name %in% plan.names[i,1]]$display_name <- as.character(plan.names[i,2])
# }


#Convert all columns to numeric
###################

reason.data <- data.table(reason.data)
cols <- colnames(reason.data)

#for (i in (1: length(cols))){
#  reason.data <- reason.data[,cols[i] := as.numeric(cols[i])]
#}
#View(reason.data)
#class(reason.data$wage_inflation)

####################
#reason.data$refunds <- as.numeric(reason.data$refunds)
#reason.data$total_proj_adec_pct <- as.numeric(reason.data$total_proj_adec_pct)

############ Pull Source datat for each state plan
#x <- (unique(reason.data$plan_name))
#length(x)
#x[3]

#y <-  data.table(pullSourceData(pl, x[1], 2017))
#y <- data.tabel(y[,1:40])

#x <- for(i in (2:length(x))){
#  z <-  data.table(pullSourceData(pl, x[6], 2017))
#  z <- z[,1:40]
#  y <- rbind(data.table(y), z, fill=T)
#}
#View(y)

######## Comparing Reason to PPD Data #######

##Compare Reason data to PPD for several variables after 2004
##With t.test
#urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-latest_06.27.20.csv"
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-Q2-2022.csv"

#Add arr, total NC, ER_cont, EE_Cont
#Add duplicates (last 2 years))
#Add ADEC rate

PPD<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
PPD <- data.table(PPD)
PPD <- PPD %>% filter(AdministeringGovt == 0)
# PPD$AdministeringGovt
# #Remove Colorado State and Shool plan
# PPD <- PPD[PlanName != "Colorado State and School"]
# PPD <- data.table(PPD[AdministeringGovt == 0] %>%
#                    select(fy, PlanName, StateName, ActLiabilities_GASB, MktAssets_net, ActAssets_GASB,
#                           ActFundedRatio_GASB,
#                           InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
#                           PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, PercentReqContPaid,
#                           expense_TotBenefits) %>% arrange(PlanName))
# 
# #View(PPD[PlanName == "Texas ERS"])
# reason.data <- data.table(reason.data)
# #View(unique(PPD$PlanName))
# #View(unique(reason.data$plan_name))
test <- matrix(0,1,4)
colnames(test) <- c("aal", "mva", "funded", "payroll")
test <- data.table(test)

yearx <- 2010

### Pull reason data &
### Set columns to numeric

## Reason
reason.data <- pullStateData(2001)
reason.data <- filterData2(reason.data, 2010)
reason.data <- data.table(reason.data)

##
reason.data$mva <- as.numeric(reason.data$mva)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$payroll <- as.numeric(reason.data$payroll)

#reason.data$funded_ratio <- reason.data$ava/reason.data$aal
#View(reason.data$aal)

## PPD
PPD$ActAssets_GASB  <- as.numeric(PPD$ActAssets_GASB)
PPD$MktAssets_net  <- as.numeric(PPD$MktAssets_net )
PPD$ActFundedRatio_GASB  <- as.numeric(PPD$ActFundedRatio_GASB)
PPD$ActLiabilities_GASB  <- as.numeric(PPD$ActLiabilities_GASB)
PPD$payroll  <- as.numeric(PPD$payroll)


### First T-Test

t.test(na.omit(PPD[fy >  yearx]$ActLiabilities_GASB)*1000,
       na.omit(reason.data[year > yearx]$aal),paired=FALSE,var.equal=FALSE)

unique(PPD$PlanName)
unique(reason.data$plan_name)

# 
# PPD$ActFundedRatio_GASB <- PPD$ActAssets_GASB/PPD$ActLiabilities_GASB
# # 
# # #View(colnames(PPD))
# # ##### independent 2-group T-test
# # #https://data-flair.training/blogs/t-tests-in-r/
# # #setting var.equal=FALSE will default to Welch's T-test (two-sample test with unequaL variance and sample size)
# # wilcox.test() - two-sample test with non-normal distribution
# #https://uc-r.github.io/t_test
# #t.test(log(percollege) ~ state, data = df)
# #wilcox.test(percollege ~ state, data = df)
# # 
# # ### AAL
test$aal <- t.test(na.omit(PPD[fy > yearx]$ActLiabilities_GASB)*1000, na.omit(reason.data[year > yearx]$aal),paired=FALSE,var.equal=FALSE)$p.value

# # 
# # 
# # ### MVA
test$mva <- t.test(na.omit(PPD[fy > yearx]$MktAssets_net)*1000, reason.data[year > yearx]$mva)$p.value
# # 
# # ### Funded
test$funded <- t.test(na.omit(PPD[fy > yearx]$ActFundedRatio_GASB), na.omit(reason.data[year > yearx]$funded_ratio))$p.value
# 
# # ### Payroll
test$payroll <- t.test(na.omit(PPD[fy > yearx]$payroll)*1000, na.omit(reason.data[year > yearx]$payroll))$p.value

test

##############