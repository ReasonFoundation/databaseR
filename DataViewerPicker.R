### Reason Database Viewer ###
#(by Anil Niraula)#

#https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
#****Create graph(s) tab showing Inv.Returns using BBC package: https://bbc.github.io/rcookbook/
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
#library(shinyFiles)
library(DT)
library(plotly)
#devtools::install_github("ropensci/plotly")

#DF <- data.table(Fiscal_Year = seq(2001, 2019, by =1))
#DF <- DF[,var1 := data.table(rnorm(19, 30, 5))]
#DF <- DF[,var2 := data.table(rnorm(19, 60, 10))]
#DF <- data.frame(DF)

#ggplot() +
#  geom_col(data=DF %>% pivot_longer(starts_with("var")),
#           mapping = aes(x=Fiscal_Year, y=value,group = 1, fill = "orangered1"),
#           color = "black", position = "dodge2")+
#  theme_bw()

pl <- planList()
states <- as.character(unique(pl[,3]))
plans <- as.character(unique(pl[,2]))
#View(pullData(pl[state=="New Mexico"], pl[state=="New Mexico"]$display_name))
#View(pl)

#palette_reason$categorical[[3]]
#reason_color_pal("categorical")
palette_reason <- data.table(
  Orange = "#FF6633", 
  LightOrange = "#FF9900",
  DarkGrey = "#333333", 
  SpaceGrey = "#A69FA1",
  DarkBlue = "#0066CC",
  GreyBlue = "#6699CC", 
  Yellow = "#FFCC33", 
  LightBlue = "#66B2FF", 
  SatBlue = "#3366CC", 
  Green = "#669900",
  LightGreen = "#00CC66",
  Red = "#CC0000",
  LightRed = "#FF0000")
#palette

pullSourceData <- function(plan_name){
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  # define the query to retrieve the plan data
  
  if(str_count(plan_name)<6){
    query <- paste("select * from pull_data_state_only()
where year > '2001'
and attribute_name in ('1 Year Investment Return Percentage',
'1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'Actuarially Accrued Liabilities Dollar',
'Total Normal Cost Percentage',
'Covered Payroll Dollar',
'Payroll Growth Assumption',
'Total Benefits Paid Dollar')")}else{
  
  plan_id <- pl$id[pl$display_name == plan_name]
  query <- paste("select * from pull_plan_data(",plan_id,")")
  #paste0("select * from pull_plan_data('", str_replace(plan_name,"'", "''"), "')")
}
  
  ###################
  
  result <- RPostgres::dbSendQuery(con, query)
  #RPostgres::dbBind(result, list(1))
  all_data <- RPostgres::dbFetch(result) %>%
    janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  
  all_data %>%
    dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%  # group by everything other than the value column.
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%  # build group index
    tidyr::spread(.data$attribute_name, .data$attribute_value, convert = TRUE) %>%    # spread
    dplyr::select(-.data$row_id) %>%  # drop the index
    janitor::clean_names()
}

#View(pullSourceData("Employee Retirement System of Hawaii"))
##Pull state Data only

pullStateData <- function(year){
  
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  
  query <- paste("select * from pull_data_state_only()
where year > '", paste(year-1), "'
and attribute_name in ('1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'Market Value of Assets Dollar',
'Actuarial Value of Assets GASB Dollar',
'Actuarially Accrued Liabilities Dollar',
'Actuarial Funded Ratio Percentage',
'Unfunded Actuarially Accrued Liabilities Dollar',
'Employee Contribution Dollar',
'Employee Normal Cost Percentage',
'Employer Normal Cost Dollar',
'Employer Contribution Regular Dollar',
'Total Contribution Dollar',
'Total Normal Cost Percentage',
'Total Amortization Payment Percentage',
'Covered Payroll Dollar',
'Actuarially Required Contribution Dollar',
'Actuarially Required Contribution Paid Percentage',
'Employers Projected Actuarial Required Contribution Percentage of Payroll',
'Payroll Growth Assumption',
'Type of Employees Covered',
'Total Pension Liability Dollar',
'Amortizaton Method',
'Actuarial Cost Method in GASB Reporting',
'Number of Years Remaining on Amortization Schedule',
'Actuarial Cost Method in GASB Reporting',
'Wage Inflation',
'Total Benefits Paid Dollar')")
  
  result <- RPostgres::dbSendQuery(con, query)
  #RPostgres::dbBind(result, list(1))
  all_data <- RPostgres::dbFetch(result) %>%
    janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  
  all_data %>%
    dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%  # group by everything other than the value column.
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%  # build group index
    tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value) %>%# CHANGED to pivot
    dplyr::select(-.data$row_id) %>%  # drop the index
    dplyr::arrange(display_name, year) %>%
    janitor::clean_names()
    
}

#View(pullStateData(2010))
##Add columns
##Convert to Wide format
##Why 112 state plans (which 2 are missing?)
#View(unique(all_data$display_name))

variables <- c(
"return_1yr",
"actuarial_cost_method_in_gasb_reporting",
"funded_ratio",
"actuarial_valuation_report_date",
"ava",
"mva",
"mva_smooth",#added
"aal",
"tpl",
"adec",
"adec_paid_pct",
"statutory",#NEW
"statutory_pct",#NEW
"amortizaton_method",
"asset_valuation_method_for_gasb_reporting",
"total_benefit_payments",#added
"benefit_payments",
"refunds",
"admin_exp",
"cost_structure",
"payroll",
"ee_contribution",
"ee_nc_pct",
"er_contribution",
"er_nc_pct",
"er_state_contribution",
"er_proj_adec_pct",
"other_contribution",#added
"other_additions",#added
"fy_contribution",
"inflation_assum",
"arr",
"dr",#NEW
"multidr",#NEW
"number_of_years_remaining_on_amortization_schedule",
"payroll_growth_assumption",
"total_amortization_payment_pct",
"total_contribution",
"total_nc_pct",
"total_number_of_members",
"total_proj_adec_pct",
"type_of_employees_covered",
"unfunded_actuarially_accrued_liabilities_dollar",
"wage_inflation")

#NMPERA.wide <- pullSourceData("New Mexico Educational Retirement Board")
###Columns were some plans have no data for
columns <- c("total_pension_liability_dollar", "wage_inflation",
             "payroll_growth_assumption", "other_contribution_dollar",
             "other_additions_dollar", "x1_year_investment_return_percentage",
             "amortization_method", "number_of_years_remaining_on_amortization_schedule",
             "fiscal_year_of_contribution", "statutory_payment_dollar",
             "statutory_payment_percentage", "discount_rate_assumption", "multiple_discount_rates")

#Custom Function to filter for number of variables we commonly use in pension analysis (state plans*)
filteredData <- function(plan, y, fy){
  Plan <- data.table(pullData(plan, y))
  ##Create missing columns for plans with no data for st 7 variable
  for (i in (1:length(columns))){
    if(sum((colnames(Plan) == columns[i]))==0) {
      Plan[,columns[i] := NA]}
  }
  
  if(sum(is.na(Plan$discount_rate_assumption))==0){ 
    Plan$discount_rate_assumption <- Plan$investment_return_assumption_for_gasb_reporting}
  ####
  Plan <- Plan %>%
    filter(year > fy-1)
  Plan <- Plan %>%
    select(
      year,
      plan_name = display_name,
      state,
      return_1yr = x1_year_investment_return_percentage,
      actuarial_cost_method_in_gasb_reporting,
      funded_ratio = actuarial_funded_ratio_percentage,
      actuarial_valuation_report_date,
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
      mva_smooth = market_assets_reported_for_asset_smoothing,#added
      aal = actuarially_accrued_liabilities_dollar,
      tpl = total_pension_liability_dollar,
      adec = actuarially_required_contribution_dollar,
      adec_paid_pct = actuarially_required_contribution_paid_percentage,
      statutory = statutory_payment_dollar,#NEW
      statutory_pct = statutory_payment_percentage,#NEW
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
      fy_contribution = fiscal_year_of_contribution,
      inflation_assum = inflation_rate_assumption_for_gasb_reporting,
      arr = investment_return_assumption_for_gasb_reporting,
      dr = discount_rate_assumption,#NEW
      multidr = multiple_discount_rates,#NEW
      number_of_years_remaining_on_amortization_schedule,
      payroll_growth_assumption,
      total_amortization_payment_pct = total_amortization_payment_percentage,
      total_contribution = total_contribution_dollar,
      total_nc_pct = total_normal_cost_percentage,
      total_number_of_members,
      total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
      type_of_employees_covered,
      unfunded_actuarially_accrued_liabilities_dollar,
      wage_inflation
    )
}
x <- filteredData(pl, "Illinois Teachers Retirement System", 2001)
#y <- filteredData(pl, "Employee Retirement System of Hawaii", 2001)

filteredSourceData <- function(plan_name, fy){
  Plan <- data.table(
    pullSourceData(plan_name))##Moved pullSourceData() inside this function
  Plan <- data.table(Plan %>%
                       filter(year > fy-1))
  ##Create columns that don't have any data
  for (i in (1:length(columns))){
    if(sum((colnames(Plan) == columns[i]))==0) {
      Plan[,columns[i] := NA]}
  }
  if(sum(is.na(Plan$discount_rate_assumption))==0){ 
    Plan$discount_rate_assumption <- Plan$investment_return_assumption_for_gasb_reporting}
  ####
  Plan <- Plan %>%
    select(
      year,
      plan_name = display_name,
      state,
      data_source_name,#Added
      return_1yr = x1_year_investment_return_percentage,
      actuarial_cost_method_in_gasb_reporting,
      funded_ratio = actuarial_funded_ratio_percentage,
      actuarial_valuation_report_date,
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
      mva_smooth = market_assets_reported_for_asset_smoothing,#added
      aal = actuarially_accrued_liabilities_dollar,
      tpl = total_pension_liability_dollar,
      adec = actuarially_required_contribution_dollar,
      adec_paid_pct = actuarially_required_contribution_paid_percentage,
      statutory = statutory_payment_dollar,#NEW
      statutory_pct = statutory_payment_percentage,#NEW
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
      fy_contribution = fiscal_year_of_contribution,
      inflation_assum = inflation_rate_assumption_for_gasb_reporting,
      arr = investment_return_assumption_for_gasb_reporting,
      dr = discount_rate_assumption,#NEW
      multidr = multiple_discount_rates,#NEW
      number_of_years_remaining_on_amortization_schedule,
      payroll_growth_assumption,
      total_amortization_payment_pct = total_amortization_payment_percentage,
      total_contribution = total_contribution_dollar,
      total_nc_pct = total_normal_cost_percentage,
      total_number_of_members,
      total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
      type_of_employees_covered,
      unfunded_actuarially_accrued_liabilities_dollar,
      wage_inflation
    )
}

#ILTRS.wide <- filteredData("Illinois Teachers Retirement System")
#View(ILTRS.wide)
#NMPERA.wide <- filteredSourceData(NMPERA.wide, 2001)
#View(NMPERA.wide)
#View(filteredData(pl, "CalPERS - California Public Employees Retirement Fund", 2001))
#x <- data.table(filteredData(pl, "New Mexico Public Employees Retirement Association", 2001))
#View(x)

###For plan-by-plan data shiny app connects directly to the database
###For aggregate (state/US level analysis) data it uses imported csv file.
##Download Reason Data ----------------------------------------------------

##State-level data from ReasonGitHub

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

##Load R scrip from GitHub
#https://www.r-bloggers.com/reading-an-r-file-from-github/
library(devtools)
library(roxygen2)
pullSourceData.test <- source_url("https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/PullSourceData.R")
#View(pullSourceData.test$value("New Mexico Educational Retirement Board"))
#NMPERA.wide <- pullSourceData("New Mexico Educational Retirement Board")

library(devtools)
library(roxygen2)
pullStateData.test <- source_url("https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/pullStateData.R")
#View(pullStateData.test$value(2001))
#View(pullStateData.test$value())

#View(reason.data)
###Download full database or data for a specific plan in "R":
#RUN THIS:
#reason.data <- pullData(pl, pl$display_name)
#reason.data <- data.table(reason.data)
#View(reason.data)

#########
# Filter Downloaded Data -------------------------------------------------------------

##Filter out reason data for variables we commonly use for pension analysis

reason.data <- reason.data %>%
  #########
select(
  year,
  plan_name = display_name,
  state,
  return_1yr = x1_year_investment_return_percentage,
  actuarial_cost_method_in_gasb_reporting,
  funded_ratio = actuarial_funded_ratio_percentage,
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

reason.data$arr <- as.numeric(reason.data$arr)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$return_1yr <- as.numeric(reason.data$return_1yr)
reason.data$year <- as.numeric(reason.data$year)
#View(reason.data)

#Label state and local plans with*
urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(plan.names)

pl <- data.table(pl)
for (i in 1:plan.names[,.N]){
  pl[display_name %in% plan.names[i,1]]$display_name <- as.character(plan.names[i,2])
}

##Calculating quantiles for ARR
reason.data <- data.table(reason.data)
perc.return <- matrix(0,19,5)
data <- reason.data[,quantile(na.omit(arr)), by=list(year)]$V1
for (i in 1:19) {
  perc.return[i,] <- data[(i+(i-1)*4):(i*5)]
}
perc.return <- data.table(perc.return)
colnames(perc.return) <- c("5th", "25th", "50th", "75th", "95th")
#View(perc.return)

##Save 30-Y Treasury data (annual average)
tr30 <- c(0.0688016, 0.067063095,	0.0660676,	0.055768,
          0.058722311,	0.059406375,	0.054948387,	0.0543,
          0.053007589,	0.051715179,	0.050422768,	0.049130357,
          0.048382869,	0.042775299,	0.0407664,	0.042510757,
          0.039108,	0.0292168,	0.0344616,	0.0333816,	0.02840996,
          0.025944,	0.0289404,	0.031115663,	0.0258)

#reason.data[, median(na.omit(return_1yr)), by=list(year)]$V1
#View(reason.data)

#Geomean function
#x <- c(0.1, 0.05, -0.04, NA)
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}

#Find first non-zero value function
first.nan <- function(x) {
  first(x[which(!is.na(x))])
}
#geomean(x)

##Create separate theme() for ggplot
plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0.1, 3,0,3, "cm"),
                               axis.text.y = element_text(size=10, color = "black"),
                               axis.text.x = element_text(size=10, color = "black", angle = 90, hjust = 1, vjust = 0.5),
                               legend.title = element_text(size = 8, colour = "white", face = "bold"))

##Ensure all variables are numeric
reason.data$mva <- as.numeric(reason.data$mva)
reason.data$return_yr <- as.numeric(reason.data$return_1yr)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$arr <- as.numeric(reason.data$arr)
reason.data$payroll <- as.numeric(reason.data$payroll)
reason.data$payroll_growth_assumption <- as.numeric(reason.data$payroll_growth_assumption)
reason.data$total_nc_pct <- as.numeric(reason.data$total_nc_pct)
reason.data$benefit_payments <- as.numeric(reason.data$benefit_payments)
reason.data$refunds <- as.numeric(reason.data$refunds)
reason.data$total_proj_adec_pct <- as.numeric(reason.data$total_proj_adec_pct)

########
##Compare Reason data to PPD for several variables after 2004
##With t.test
#urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/ppd-data-latest_06.27.20.csv"
#PPD<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#PPD <- setDT(PPD)
#PPD$AdministeringGovt
#Remove Colorado State and Shool plan
#PPD <- PPD[PlanName != "Colorado State and School"]
#PPD <- data.table(PPD[AdministeringGovt == 0] %>%
#                    select(fy, PlanName, StateName, ActLiabilities_GASB, MktAssets_net,
#                           InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
#                           PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, PercentReqContPaid,
#                           expense_TotBenefits) %>% arrange(PlanName))
#View(PPD[PlanName == "Texas ERS"])

#View(unique(PPD$PlanName))
#View(unique(reason.data$plan_name))
#test <- matrix(0,1,4)
#colnames(test) <- c("aal", "mva", "arr", "payroll")
#test <- data.table(test)
#test$aal <- as.numeric(t.test(is.na(PPD[fy > 2004]$ActLiabilities_GASB), is.na(reason.data[year > 2004]$aal))$p.value)
#test$mva <- t.test(is.na(PPD[fy > 2004]$MktAssets_net), is.na(reason.data[year > 2004]$mva))$p.value
#test$arr <- t.test(is.na(PPD[fy > 2004]$InvestmentReturnAssumption_GASB), is.na(reason.data[year > 2004]$arr))$p.value
#test$payroll <- t.test(is.na(PPD[fy > 2004]$payroll), is.na(reason.data[year > 2004]$payroll))$p.value
#View(test)

reason.data <- data.table(reason.data)
payrollUS <- reason.data[, sum(na.omit(payroll)), by=list(year)] %>% arrange(year)
aalUS <- reason.data[, sum(na.omit(aal)), by=list(year)] %>% arrange(year)
payrollUS <- reason.data[, sum(na.omit(payroll)), by=list(year)] %>% arrange(year)
#View(aalUS)
years <- seq(2001, 2018, by = 1)
aal.pct.ch <- matrix(0,1,17)
#aal.pct.ch

for(i in(1:17)){
  aal.pct.ch[,i] <-  (aalUS[year == years[i+1]]$V1-aalUS[year == years[i]]$V1)/aalUS[year == years[i]]$V1
}
colnames(aal.pct.ch) <- seq(2002, 2018, by = 1)
aal.pct.ch <- t(aal.pct.ch)
colnames(aal.pct.ch) <- c("aal.pct.change")
#View(aalUS$V1)

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/GDP.FRED.csv"
#SOurce: https://fred.stlouisfed.org/series/GDP#0
gdp.pct <- data.table(
  read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
gdp.pct <- gdp.pct[year>2001 & year < 2019]
#View(gdp.pct)
#View(aal.pct.ch)
gdp.aal <- cbind(gdp.pct,round(aal.pct.ch*100,1))
#View(gdp.aal)
gdp.aal$gdp.pct.change <- as.numeric(gdp.aal$gdp.pct)
gdp.aal[,avg.aal := mean(aal.pct.ch)*100]
gdp.aal[,avg.gdp := mean(gdp.pct.change)]

gdp.pct <- data.table(gdp.pct)

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/GDP.csv"
#Source: https://fred.stlouisfed.org/series/GDP#0
gdp.level <- data.table(
  read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
gdp.level <- gdp.level[DATE>2001 & DATE < 2019]
#View(gdp.level)
gdp.level$GDP <- as.numeric(gdp.level$GDP)
gdp.level$GDP
#View(aalUS)
gdp.level$GDP/1000*30
gdpvsaal.level <-(aalUS[year>2001 & year < 2019]$V1/1000000000000)/(gdp.level$GDP/1000)
gdpvsaal.level <- data.table(gdpvsaal.level)
gdpvsaal.level[,year := seq(2002, 2018, by = 1)]
#View(gdpvsaal.level)
gdpvsaal.30 <-(aalUS[year>2001 & year < 2019]$V1/1000000000000)/((gdp.level$GDP/1000)*30)
gdpvsaal.30 <- data.table(gdpvsaal.30)
gdpvsaal.30[,year := seq(2002, 2018, by = 1)]
#View(gdpvsaal.30)
#gdp.pct
#View(payrollUS)
#View(unique(reason.data$plan_name))
#TxERS <- data.table(reason.data[plan_name == "Texas Employees Retirement System"])
#TxERS[year == 2010]$payroll
#TxERS[year == 2012]$payroll

#1-(TxERS[year == 2012]$payroll/TxERS[year == 2010]$payroll)
#(TxERS[year == 2012]$payroll-TxERS[year == 2010]$payroll)/TxERS[year == 2010]$payroll
#View(TxERS)

######Shiny app[interface] ----------------------------------------------

ui <- fluidPage(
  titlePanel("Reason Database Viewer (V3.0)"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(
      img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 50),
      br(),
      br(),
      selectInput("x", "Select State", choices = states),
      uiOutput("secondSelection"),
      #ADD slider input to choose year range
      sliderInput('year', 'Select Starting Year', min = 1990, max = 2019, value = 2001, sep = ""),
      uiOutput("thirdSelection"),
      uiOutput("forthSelection"),
      em("NOTES: "),
      br(),
      em("Filtered data is available for major `state` plans."),
      br(),
      em("Update#1 Data sources are displayed in Source tab."),
      br(),
      em("Update#2 Discount rate assumption data added to Filtered option."),
      br(),
      em("Update#3 Added multiple column selection for Filtered data."),
      br(),
      br(),
      textOutput('plot_2019Updates'),
      # Button
      downloadButton("downloadData", "Download"),#, width = 3
      actionButton("show_note", "Note")
    ),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel('Table', DT::DTOutput('plot_DataPull')),
        tabPanel('Columns', DT::DTOutput('plot_Variables'),
                 tableOutput("meta_table")),
        tabPanel('Source', DT::DTOutput('plot_SourceDataPull')),
        tabPanel("UAL",plotly::plotlyOutput("plot_Filtered"), 
                 plotly::plotlyOutput("plot_Filtered_UAL")),
        tabPanel("Inv.Returns", plotly::plotlyOutput("plot_Filtered_Returns"),
                 plotly::plotlyOutput("plot_Filtered_ARR")),
        tabPanel("Contributions", plotly::plotlyOutput("plot_Filtered_Contr"),
                 plotly::plotlyOutput("plot_Filtered_CashFlow"))
      )
    )
  )
)
##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  note_text <- paste0("This shiny app allows you to browse through Reason database by", sep="\n", "\n",
                      "selecting a state & pension plan.", sep="\n",
                      "Go to 'Table' & 'Columns' tabs to see data for chosen plan (to save use download button). ", sep="\n",
                      "For more granular data choose 'Filtered' optioin", sep="\n",
                      "(available for all state & some municipal plans).", sep="\n",
                      "Go to 'UAL' & 'Inv.Returns/ADEC' tabs for some historical graphs.", sep = "\n")
  
  observeEvent(input$show_note,{
    showModal(modalDialog(note_text))
  })
  
  output$secondSelection <- renderUI({
    pl <- data.table(pl)
    pl1 <- data.table(pl %>% filter(state == input$x))
    state <- data.table(pl1[display_name %in% plan.names$state.plans.nm.rev]$display_name)
    local <- data.table(pl1[!display_name %in% plan.names$state.plans.nm.rev]$display_name)
    selectInput("y", "Select Plan", choices = list(c(state[1]$V1),state = state$V1, local = local$V1), 
                selected = ifelse(!is.na(state$V1[1]),1, first(local$V1)))
  })
  
  #Create a dinamic buttons in the main panel depending on the pension plan chosen
  output$thirdSelection <- renderUI({
    pl1 <- pullData(pl,input$y)
    if(ncol(pl1)>100) {
      radioGroupButtons("filter", "Data", choices = c("Full", "Filtered"),
                        status = "primary")
    } else {
      radioGroupButtons("filter", "Data", choices = c("Full"),
                        status = "primary")
    }
  })
  
  output$forthSelection <- renderUI({
    if(input$filter == "Filtered"){
      pickerInput("pk", "Select Columns (Filtered data only)", 
                                                 choices = c(variables),
                                                 selected = c(variables),
                                                 multiple = T,
                                                 options = list(`actions-box` = TRUE))
    } else {
      NULL
    }
  })
  
  PlanData <- reactive({
    if(input$filter == "Filtered"){
      UAL <- data.table(filteredData(pl, input$y, input$year))
  
    } else {
      UAL <- pullData(pl, input$y)
      UAL <- UAL %>%
        filter(year >= input$year)
    } 
    
  })

  ##Create a reactive datapull object to use for shiny graphics later

  output$plot_DataPull <- DT::renderDT({
    ###Specify data to show (Filter out variables)
    x <- data.table(PlanData())
    if(input$filter == "Filtered"){
    x <- x %>% select(year, plan_name, state, input$pk)}
    x
  })
  
  ##Create a reactive source data table
  PlanSourceData <- reactive({
    
    if(input$filter == "Filtered"){
      Plan <- data.table(
        filteredSourceData(input$y, input$year))
    } else {
      Plan <- data.table(pullSourceData(input$y)) %>%
        filter(year >= input$year)
    } 
  })
  
  output$plot_SourceDataPull <- DT::renderDT({
    x <- data.table(PlanSourceData())
    if(input$filter == "Filtered"){
    x <- x %>% select(year, plan_name, state, data_source_name, input$pk)}
    x
  })
  
  output$plot_Variables <- DT::renderDT({
    #Load reactive datapull
    x <- data.table(PlanData())
    if(input$filter == "Filtered"){
    x <- x %>% select(year, plan_name, state, input$pk)}
    x <- data.table(colnames(x))
    colnames(x) <- c("Variables")
    x
  })
  
  output$plot_2019Updates <- renderText({
    Updt.2019 <- data.table(pullData(pl, input$y))
    Updt.2019 <-  min(max(Updt.2019$year), max(Updt.2019[!is.na(display_name)]$year))
    paste0("*Latest: ", Updt.2019, "FY", sep = "")
  })
  
  #Create interactive plot
  output$plot_Filtered <- plotly::renderPlotly({
    
    #Call on a reactive data that was filtered above
    UAL <- data.table(PlanData()) 
    UAL <- data.table("Actuarial_Assets"= as.numeric(UAL$ava)/1000000000, 
                      "Actuarial_Liability"= as.numeric(UAL$aal)/1000000000,
                      "Fiscal_Year"= as.numeric(UAL$year),# referenced filtered years instead of a fixed 2001-20 sequence,
                      "UAL_AVA" = (as.numeric(UAL$aal)-as.numeric(UAL$ava))/1000000000)
    
    UAL <- data.frame(UAL)
    #View(UAL)
    #Graphics manual: https://bbc.github.io/rcookbook/
    #https://github.com/ReasonFoundation/pensionviewr/blob/master/README.Rmd
    
    p <- ggplot() +
      ggtitle(label = paste0("Plan: ", input$y))+
      
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), color="Unfunded Actuarial Liability"), fill = "gray88", size = 0.75)+
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Assets,2)), fill = "white", size = 0.75)+
      
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Actuarial_Liability, 
                              color="Actuarial Accrued Liability", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>AAL: $",round(Actuarial_Liability,1), "B")),
                size = 1.00)+
      # geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Actuarial_Liability,2), NA), 
      #                          color="Actuarial Liability", group = 1,
      #                          text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                        "<br>Liability: $",ifelse(Act_Est == 1,round(Actuarial_Liability,1), NA),"B", "\n",  "\n",  
      #                                        "*Actuarial Accrued Liability", "\n",  
      #                                        "is the total value of the pension benefits", "\n",  
      #                                        "promised to employees to date, which is", "\n",  
      #                                        "calculated by an actuary each year.")),
      #            linetype = "dotted", color = "white",size = 0.75)+
      #  ##Adding Points
      #  geom_point(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), 
    #                           color="Actuarial Liability", group = 1,
    #                           text = paste0("Fiscal Year: ", Fiscal_Year,
    #                                         "<br>Liability: $",round(Actuarial_Liability,1),"B", "\n",  "\n",  
    #                                         "*Actuarial Accrued Liability", "\n",  
    #                                         "is the total value of the pension benefits", "\n",  
    #                                         "promised to employees to date, which is", "\n",  
    #                                         "calculated by an actuary each year.")),
    #             size = 1.00)+
    geom_line(data=UAL, aes(x=Fiscal_Year, y=Actuarial_Assets, 
                            color="Actuarial Value of Assets", group =1,
                            text = paste0("Fiscal Year: ", Fiscal_Year,
                                          "<br>AVA: $",round(Actuarial_Assets,1), "B")),
              size = 1.00)+
      # , "\n",  "\n", 
      #                                        "*Market Value of Assets", "\n", 
      #                                        "is the real value of plan’s total assets,", "\n", 
      #                                        "measured by the price of selling all assets ", "\n",
      #                                        "in an orderly transaction at that date.")), 
      #            size = 0.75)+
      #  geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Market_Assets,2), NA), 
      #                          color="Assets", group = 1,
      #                          text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                        "<br>Assets: $",ifelse(Act_Est == 1,round(Market_Assets,1), NA), "B", "\n",  "\n", 
      #                                        "*Market Value of Assets", "\n", 
    #                                        "is the real value of plan’s total assets,", "\n", 
    #                                        "measured by the price of selling all assets ", "\n",
    #                                        "in an orderly transaction at that date.")), 
    #            linetype = "dotted", color = "white", size = 0.75)+
    ##Adding Points
    #  geom_point(data=UAL, aes(x=Fiscal_Year, y=round(Market_Assets,2), 
    #                           color="Assets", group = 1,
    #                           text = paste0("Fiscal Year: ", Fiscal_Year,
    #                                         "<br>Assets: $",round(Market_Assets,1), "B", "\n",  "\n", 
    #                                         "*Market Value of Assets", "\n", 
    #                                         "is the real value of plan’s total assets,", "\n", 
    #                                         "measured by the price of selling all assets ", "\n",
    #                                         "in an orderly transaction at that date.")),  
    #             size = 1.00)+
    #geom_point(data=UAL, aes(x=Fiscal_Year[20], y=Actuarial_Liability[20]), size = 0.75)+
    #geom_point(data=UAL, aes(x=Fiscal_Year[20], y=Market_Assets[20]), size = 0.75)+
    #scale_colour_manual(values=c("orangered2", "royalblue3","white", "white"))+
    #manual color reference: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    scale_colour_manual(values=c(palette_reason$Orange, palette_reason$SatBlue, "white", "white"))+
      scale_y_continuous(labels = function(x) paste0("$",x,"B"), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    #Annotating ending UAL
    annotate("text", fontface = 'bold', size = 3.5, x=2016, y=as.numeric((UAL[16,1]/2)), size=3,
             label = paste0("FY", last(UAL$Fiscal_Year),":", sep="\n", 
                            " $",round(last((UAL$UAL_AVA)),1), "B",
                            " Pension Debt", sep="\n"))
    
    p <- ggplotly(p, frame = UAL$Fiscal_Year, tooltip = c("text"))
    p <- p %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    p
  })
  
  #Adding some meta data outputs (table)
  output$meta_table <- renderTable({
    Data <- data.table(PlanData())
    Data %>%
      summarize(
        nb_rows = Data[,.N],
        nb_cols = length(colnames(Data)),
        periods = paste(min(as.numeric(year)), " to ", max(as.numeric(year))),
        avg_return = mean(na.omit(as.numeric(x1_year_investment_return_percentage))),
        start_ual = paste0(min(as.numeric(year)), ": ", 
                           (round(na.omit(as.numeric(Data[year == min(year)]$actuarially_accrued_liabilities_dollar-
                                                       Data[year == min(year)]$actuarial_value_of_assets_gasb_dollar))/1000000,2)
                           ),"M"),
        end_ual = paste0(last(as.numeric(year)), ": ", 
                         (round(na.omit(as.numeric(Data[year == last(year)]$actuarially_accrued_liabilities_dollar-
                                                     Data[year == last(year)]$actuarial_value_of_assets_gasb_dollar))/1000000,2)
                         ),"M")
      )
  })
  output$plot_Filtered_UAL <- plotly::renderPlotly({
    
    UAL <- data.table(PlanData())
    UAL <- data.table("Actuarial_Assets"= (UAL$ava)/1000000000, 
                      "Actuarial_Liability"= (UAL$aal)/1000000000,
                      "Fiscal_Year"= as.numeric(UAL$year),# referenced filtered years instead of a fixed 2001-20 sequence,
                      "UAL_AVA" = ((UAL$aal)-(UAL$ava))/1000000000,
                      "UAL_AVA2" = ifelse((UAL$aal<UAL$ava),1,0)
    )
    
    
    #View(UAL)
    
    #Shift green color to the right
    #yr1 <- max(UAL[UAL_AVA2==1]$Fiscal_Year)
    #UAL[UAL_AVA2==0 &Fiscal_Year == (yr1+1)]$UAL_AVA2 <- 1
    
    UAL <- data.frame(UAL)
    
    u <- ggplot() +
      geom_area(data=UAL, aes(x=Fiscal_Year, y=UAL_AVA, 
                              color="Unfunded Actuarial Liability", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>UAL: $",round(UAL_AVA,2), "B")),
                fill = palette_reason$Orange#, alpha = 1, size = 1
      )+
      
      # geom_area(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,round(UAL_AVA,2), NA)), fill = "white", alpha = 1)+
      # geom_area(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,round(UAL_AVA,2), NA)), fill = "green3", alpha = 1, size = 1)+  
      #  geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,0, NA)), color = "green3", size = 0.1)+
      #  geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,round(UAL_AVA,2), NA)), color = "green3", size = 0.1)+
      
      scale_colour_manual(values=c(palette_reason$Orange))+
      
      scale_y_continuous(labels = function(x) paste0("$",x,"B"), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    u <- ggplotly(u, tooltip = c("text"))
    u <- u %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    u
  })
  
  output$plot_Filtered_Returns <- plotly::renderPlotly({
    
    UAL3 <- data.table(PlanData()) 
    
    #View(state.plans[plan_name == "Alabama Employees' Retirement System (ERS)"]$return_1y) 
    geomean <- function(x) {
      x <- as.vector(na.omit(x))
      x <- x +1
      exp(mean(log(x)))-1 
    }
    returns <- as.numeric(UAL3$return_1yr)
    nyear <- 10
    rolling <- geomean(returns[1:nyear])
    n <- length(na.omit(returns))-nyear
    #Geomean function
    for(i in 1:n){
      rolling <- rbind(rolling, geomean(returns[(i+1):(i+nyear)]))
    }
    rolling <- data.table(rolling)
    
    tr30 <- c(0.0688016, 0.067063095,	0.0660676,	0.055768,
              0.058722311,	0.059406375,	0.054948387,	0.0543,
              0.053007589,	0.051715179,	0.050422768,	0.049130357,
              0.048382869,	0.042775299,	0.0407664,	0.042510757,
              0.039108,	0.0292168,	0.0344616,	0.0333816,	0.02840996,
              0.025944,	0.0289404,	0.031115663,	0.0258)
    tr30 <- data.table(tr30)
    n <- tr30[,.N]
    
    UAL4 <- data.table("Assumed_Return"= as.numeric(UAL3$arr), 
                       "Actual_Return"= as.numeric(UAL3$return_1yr),
                       "Fiscal_Year"= as.numeric(UAL3$year)
    )
    
    UAL4 <- data.table(rbind.fill(rolling, UAL4))
    UAL4[(UAL4[!is.na(Actual_Return),.N]+1):(UAL4[!is.na(Actual_Return),.N]+rolling[,.N])]$V1<- UAL4[(1:rolling[,.N])]$V1
    UAL4 <- UAL4[!(1:rolling[,.N])]
    # UAL4 <- data.table(UAL4[, Tr30 := tr30[(n-UAL4[!is.na(Actual_Return),.N]):last]])
    #View(UAL4)
    UAL4$Fiscal_Year <- as.numeric(UAL4$Fiscal_Year)
    
    UAL4 <- data.frame(UAL4)
    #View(UAL)
    k <- ggplot() +
      ggtitle(label = paste0("Plan: ", input$y))+
      geom_line(data=UAL4, aes(x=Fiscal_Year, y=Assumed_Return, 
                               color="Assumed Rate of Return", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>ARR: ",round(Assumed_Return,3)*100, "%")),
                size = 1.00)+
      geom_line(data=UAL4, aes(x=Fiscal_Year, y=Actual_Return, 
                               color="Market Valued Returns (Actual)", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>MVA Return: ",round(Actual_Return,3)*100, "%")),
                size = 1.00)+
      geom_line(data=UAL4, aes(x=Fiscal_Year, y=V1, 
                               color="10-Year Geometric Rolling Average", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>10Y Geomean: ",round(V1,3)*100, "%")),
                size = 1.00)+
      scale_colour_manual(values=c(palette_reason$Orange, palette_reason$SatBlue, palette_reason$SpaceGrey))+
      scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "",
                         breaks = seq(-0.28, 0.26, by = 0.04), limits = c(-0.28, 0.26))+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(min(UAL4$Fiscal_Year), 2019, by = 1), limits = c(min(UAL4$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    k <- ggplotly(k, tooltip = c("text"))
    k <- k %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.01))
    k
  })
  
  output$plot_GDPvsAAL <- plotly::renderPlotly({
    Data <- data.frame(gdp.aal)
    Data[,1] <- as.numeric(Data[,1])
    Data[,2] <- as.numeric(Data[,2])
    Data[,3] <- as.numeric(Data[,3])
    #View(gdp.aal)
    
    #View(UAL)
    k <- ggplot() +
      ggtitle(label = paste0("State AAL vs GDP Change (Y%)"))+
      geom_line(data=Data, aes(x=Data[,1], y=Data[,2], 
                               color="US GDP Change", group =1,
                               text = paste0("Fiscal Year: ", Data[,1],
                                             "<br>Annual US GDP Change: ",Data[,2], "%")),
                size = 1.00, fill="royalblue")+
      geom_line(data=Data, aes(x=Data[,1], y=Data[,3], 
                               color="State-Level AAL Change", group =1,
                               text = paste0("Fiscal Year: ", Data[,1],
                                             "<br>Annual State-Level AAL Change: ",Data[,3], "%")),
                size = 1.00)+
      geom_line(data=Data, aes(x=Data[,1], y=Data[,4], 
                               color="Average AAL Change", group =1,
                               text = paste0("Fiscal Year: ", Data[,1],
                                             "<br>Average AAL Change: ",round(Data[,4],1), "%")),
                size = 1.00)+
      geom_line(data=Data, aes(x=Data[,1], y=Data[,5], 
                               color="Average GDP Change", group =1,
                               text = paste0("Fiscal Year: ", Data[,1],
                                             "<br>Average GDP Change: ",round(Data[,5],1), "%")),
                size = 1.00)+
      scale_colour_manual(values=c(palette_reason$Orange, palette_reason$Blue, palette_reason$LightOrange, palette_reason$LightBlue))+
      scale_y_continuous(labels = function(x) paste0(x,"%"), name = "",
                         breaks = seq(-3, 10, by = 1), limits = c(-3, 10))+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(2002, 2018, by = 1), limits = c(2002, 2018))+
      theme_bw()+
      plotTheme#+
    
    k <- ggplotly(k, tooltip = c("text"))
    k <- k %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.01))
    k
  })
  
  output$plot_GDPlevel <- plotly::renderPlotly({
    Data <- data.frame(gdpvsaal.level)
    Data[,1] <- as.numeric(Data[,1])
    
    Data2 <- data.frame(gdpvsaal.30)
    Data2[,1] <- as.numeric(Data2[,1])
    #View(Data)
    #View(Data2)
    #View(gdp.aal)
    
    #View(UAL)
    k <- ggplot() +
      ggtitle(label = paste0("State AAL-to-GDP Level Ratio"))+
      geom_line(data=Data, aes(x=Data[,2], y=Data[,1], 
                               color="AAL-to-GDP Ratio", group =1,
                               text = paste0("Fiscal Year: ", Data[,2],
                                             "<br>AAL-to-GDP Ratio: ",round(Data[,1],4))),
                size = 1.00, fill=palette_reason$LightBlue)+
      geom_line(data=Data2, aes(x=Data2[,2], y=Data2[,1], 
                                color="AAL-to-GDP(x30) Ratio", group =1,
                                text = paste0("Fiscal Year: ", Data2[,2],
                                              "<br>AAL-to-GDP(x30) Ratio: ",round(Data2[,1],4))),
                size = 1.00, fill=palette_reason$LightBlue)+
      scale_colour_manual(values=c(palette_reason$Orange, palette_reason$LightOrange))+
      scale_y_continuous(labels = function(x) paste0(round(x,3)), name = "",
                         breaks = seq(0, 0.3, by = 0.03), limits = c(0, 0.3))+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(2002, 2018, by = 1), limits = c(2002, 2018))+
      theme_bw()+
      plotTheme#+
    
    k <- ggplotly(k, tooltip = c("text"))
    k <- k %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    k
  })
  
  output$plot_Filtered_Contr <- plotly::renderPlotly({
    UAL5 <- data.table(PlanData()) 
    #View(state.plans[plan_name == "Alabama Employees' Retirement System (ERS)"]$return_1y) 
    
    UAL5 <- data.table("ADEC"= as.numeric(UAL5$adec), 
                       "ADEC_Paid"= (as.numeric(UAL5$adec)*as.numeric(UAL5$adec_paid_pct)), 
                       "Fiscal_Year"= as.numeric(UAL5$year)
    )
    #View(UAL5)
    UAL5 <- na.omit(UAL5)
    UAL5$ADEC <- as.numeric(UAL5$ADEC)
    UAL5$ADEC_Paid <- as.numeric(UAL5$ADEC_Paid)
    #UAL5 <- na.omit(UAL5)
    
    UAL5 <- data.frame(UAL5)
    #View(UAL5)
    
    c <- ggplot() +
      ggtitle(label = paste0("Plan: ", input$y))+
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ADEC,
                              color="ADEC Not Paid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC Not Paid: $",round(ADEC/1000000,2), " $Millions")), width = 0.7, 
               fill = palette_reason$Orange
      )+
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ifelse((ADEC_Paid>ADEC), ADEC_Paid,0),
                              color="ADEC OverPaid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC OverPaid: $",round((ADEC_Paid-ADEC)/1000000,2), 
                                            " $Millions")), width = 0.7, 
               fill = palette_reason$LightGreen
      )+
      
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ifelse((ADEC_Paid<=ADEC), ADEC_Paid,ADEC),
                              color="ADEC Paid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC Paid: $",ifelse((ADEC_Paid<=ADEC), round(ADEC_Paid/1000000,2),round(ADEC/1000000,2)), 
                                            " $Millions")), width = 0.7, 
               fill = "grey80"
      )+
      #geom_col(data=UAL5, aes(x=Fiscal_Year, y=ifelse(ADEC_NotPaid>0, ADEC_NotPaid,NA),
      #                        color="ADEC OverPaid", group =1,
      #                        text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                      "<br>ADEC OverPaid: $",ADEC_NotPaid/1000000, "in $Millions")), width = 0.7, 
      #         position = "stack", fill = "green2"
      #)+
      geom_line(data=UAL5, aes(x=Fiscal_Year, y=ADEC, 
                               color="ADEC", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>ADEC: $",round(ADEC/1000000,2), " $Millions")), fill = "gold2", size=0.8
      )+
      
      scale_colour_manual(values=c("gold2", "white", "white", "white"))+
      scale_y_continuous(labels = function(x) paste0("$", round((x/1000000),2), "M"), name = "Employer Contributions vs. ADEC",
      )+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(input$year-1, (last(UAL5$Fiscal_Year)+1), by = 1), limits = c(input$year-1, (max(UAL5$Fiscal_Year)+1)))+
      theme_bw()+
      ###Adding custom theme
      #https://departmentfortransport.github.io/R-cookbook/plots.html
      plotTheme#+
    
    c <- ggplotly(c, tooltip = c("text"))
    c <- c %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    c
  })
  
  output$plot_Filtered_ARR <- plotly::renderPlotly({
    UAL6 <- data.table(PlanData()) 
    #View(state.plans[plan_name == "Alabama Employees' Retirement System (ERS)"]$return_1y) 
    #returns <- as.numeric(UAL3$return_1yr)
    UAL6 <- data.table("Median_Assumed_Return"= reason.data[, median(na.omit(arr)), by=list(year)]$V1, 
                       "Average_Assumed_Return"= reason.data[, median(na.omit(arr)), by=list(year)]$V1, 
                       "Median_Actual_Return"= reason.data[, median(na.omit(return_1yr)), by=list(year)]$V1, 
                       "Treasury"= tr30[1:UAL6[,.N]],
                       "Assumed_Return"= as.numeric(UAL6$arr), 
                       "Fiscal_Year"= as.numeric(UAL6$year))
    
    #View(UAL6)
    UAL6 <- cbind(UAL6, perc.return)
    #View(UAL6)
    UAL6 <- data.frame(UAL6)
    #View(UAL6)
    #View(UAL)
    f <- ggplot() +
      #ggtitle(label = paste0("Plan: ", input$y))+
      #geom_col(data=UAL6, aes(x=Fiscal_Year, y=Median_Assumed_Return, 
      #                         color="Median Assumed Return (U.S.)", group =1,
      #                         text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                       "<br>Median ARR (U.S.): ",round(Median_Assumed_Return,3)*100, "%")),
      #          fill = "royalblue3", size = 1.00)+
      
      
      geom_line(data=UAL6, aes(x=Fiscal_Year, y=UAL6[,10], color="75th Percentile (U.S.)", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>75th Percentile ARR (U.S.): ",round(UAL6[,10],3)*100, "%")),
                fill = "royalblue3", size = 0.75)+
      geom_line(data=UAL6, aes(x=Fiscal_Year, y=UAL6[,9], color="Median (U.S.)", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Median ARR (U.S.): ",round(UAL6[,9],3)*100, "%")),
                fill = "royalblue2", size = 0.75)+
      geom_line(data=UAL6, aes(x=Fiscal_Year, y=UAL6[,8], color="25th Percentile (U.S.)", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>25th Percentile ARR (U.S.): ",round(UAL6[,8],3)*100, "%")),
                fill = "royalblue1", size = 0.75)+
      
      #geom_line(data=UAL6, aes(x=Fiscal_Year, y=Treasury, 
      #                         color="30Y Treasury (U.S.)", group =1,
      #                         text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                       "<br>30Y Treasury (U.S.): ",round(Treasury,3)*100, "%")),
      #          size = 1.00)+
      #geom_line(data=UAL6, aes(x=Fiscal_Year, y=Median_Actual_Return, 
      #                         color="Median Actual Return (U.S.)", group =1,
      #                         text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                       "<br>Median Actual Return (U.S.): ",round(Median_Actual_Return,3)*100, "%")),
      #          size = 1.00)+
    geom_point(data=UAL6, aes(x=Fiscal_Year, y=Assumed_Return, 
                              color="Plan's Assumed Return", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Plan's Assumed Return: ",round(Assumed_Return,3)*100, "%")),
               
               size = 1.5)+
      geom_line(data=UAL6, aes(x=Fiscal_Year, y=Assumed_Return, 
                               color="Plan's Assumed Return", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Plan's Assumed Return: ",round(Assumed_Return,3)*100, "%")),
                
                size = 0.5)+
      #geom_line(data=UAL4, aes(x=Fiscal_Year, y=V1, 
      #                         color="10-Year Geometric Rolling Average", group =1,
      #                         text = paste0("Fiscal Year: ", Fiscal_Year,
      #                                       "<br>10Y Geomean: ",round(V1,3)*100, "%")),
      #          size = 1.00)+
      scale_colour_manual(values=c(palette_reason$Orange, palette_reason$SpaceGrey, palette_reason$Yellow, 
                                   palette_reason$SatBlue, palette_reason$LightOrange, palette_reason$Yellow))+
      scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "",
                         breaks = seq(0.05, 0.09, by = 0.01), limits = c(0.05, 0.09))+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(min(UAL6$Fiscal_Year), 2019, by = 1), limits = c(min(UAL6$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    f <- ggplotly(f, tooltip = c("text"))
    f <- f %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.01))
    f
  })
  
  output$plot_Filtered_CashFlow <- plotly::renderPlotly({
    UAL7 <- data.table(PlanData()) 
    #View(state.plans[plan_name == "Alabama Employees' Retirement System (ERS)"]$return_1y) 
    
    UAL7 <- data.table("Total_Benefits"= -(as.numeric(UAL7$total_benefit_payments)), 
                       "Total_Contributions"= (  as.numeric(UAL7$ee_contribution)+
                                                   as.numeric(UAL7$er_contribution)),
                       "Net_Cash_Flow" = (as.numeric(UAL7$ee_contribution)+
                                            as.numeric(UAL7$er_contribution))+
                         (as.numeric(UAL7$total_benefit_payments)),
                       "Fiscal_Year"= as.numeric(UAL7$year)
    )
    
    UAL7 <- na.omit(UAL7)
    UAL7 <- data.frame(UAL7)
    #View(UAL5)
    m <- ggplot() +
      geom_col(data=UAL7 %>% pivot_longer(starts_with(colnames(UAL7[,1:3]))), 
               mapping = aes(x=Fiscal_Year, y=value,
                             group = name, fill = name, color = name, 
                             text = paste0("Fiscal Year: ", Fiscal_Year, "<br>", paste0(name),
                                           " $",value/1000000, " in $Millions")), 
               size = 0.1, position = "dodge2")+
      scale_fill_manual(values = c(palette_reason$LightBlue, palette_reason$SatBlue, palette_reason$Orange))+
      scale_colour_manual(values=c("white", "white", "white"))+
      geom_hline(yintercept = 0)+
      scale_y_continuous(labels = function(x) paste0("$", round((x/1000000),2), "M"), name = "Annual Cash Flow",
      )+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(input$year, 2019, by = 1), limits = c(input$year, 2019))+
      theme_bw()+
      ###Adding custom theme
      #https://departmentfortransport.github.io/R-cookbook/plots.html
      plotTheme#+
    
    m <- ggplotly(m, tooltip = c("text"))
    m <- m %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    m
  })
  
  output$plot_payroll <- plotly::renderPlotly({
    #ggtitle(label = paste0("Plan: ", input$y))+
    #View(payrollUS
    UAL <- data.table(PlanData())
    #View(UAL$payroll)
    UAL <- data.table("Payroll"= (UAL$payroll/1000000000), 
                      "Fiscal_Year"= (UAL$year)
    )
    
    #View(UAL)
    UAL$Fiscal_Year <-as.numeric(UAL$Fiscal_Year)
    UAL <- data.frame(UAL)
    #View(UAL)
    u <- ggplot() +
      ggtitle(label = paste0("Plan: ", input$y))+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Payroll, 
                              color="Payroll", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Payroll: $",round(Payroll,2), " Billions")),
                color = palette_reason$LightOrange, size = 1
      )+
      
      scale_colour_manual(values=c(palette_reason$LightOrange, palette_reason$LightBlue))+
      
      scale_y_continuous(labels = function(x) paste0("$", x), name = "Payroll in $Billions")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    u <- ggplotly(u, tooltip = c("text"))
    u <- u %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.1))
    u
  })
  
  output$plot_payrollUS <- plotly::renderPlotly({
    #ggtitle(label = paste0("State Plan Payroll Growth"))
    #View(payrollUS
    UAL <- data.table(PlanData())
    #View(UAL$payroll)
    UAL <- data.table("PayrollUS"=(payrollUS$V1/1000000000),
                      "Fiscal_Year"= (UAL$year)
    )
    
    #View(UAL)
    UAL$Fiscal_Year <-as.numeric(UAL$Fiscal_Year)
    UAL <- data.frame(UAL)
    #View(UAL)
    u <- ggplot() +
      geom_line(data=UAL, aes(x=Fiscal_Year, y=PayrollUS, 
                              color="Payroll US", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Payroll US: $",round(PayrollUS,2), " Billions")),
                color = "royalblue2", size = 1
      )+
      
      
      scale_y_continuous(labels = function(x) paste0("$", x), name = "Payroll in $Billions (State Plans)")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    u <- ggplotly(u, tooltip = c("text"))
    u <- u %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.1))
    u
  })
  ###Specify data to Download
  datasetInput <- reactive({
    x <- data.table(PlanData())
    if(input$filter == "Filtered"){
    x <- x %>% select(year, plan_name, state, input$pk)}
    x
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$y,"_",input$filter, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
################################