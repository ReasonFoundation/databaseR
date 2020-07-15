####### UAL Shiny App (by Anil Niraula) #######
####### Reason Foundation
#### Data: Reason

###R code for 2019 projections: https://github.com/ReasonFoundation/databaseR/blob/master/apps/proj.2019.R

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

# Load Reason pension data ---------------------------------------------------------------
####### Download the whole database   
#pl <- planList()
#reason.data <- data.table(pullData(pl, pl$display_name)) 
#View(reason.data)
#reason.data <- reason.data %>% 
#               filter(administering_government_type == 0) %>% 
#               arrange(display_name, year)
#state.plans.nm <- unique(state.plans$display_name)
#View(state.plans)
#View(state.plans.nm)
#write.csv(reason.data, file = "/Users/anilniraula/Downloads/reason.data.state.csv")

#To filter for state-level plans
##LAST UPDATED*: 07.15.20
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/reason.data.state.csv"
reason.data2 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data2 <- data.table(reason.data2)
#View(reason.data2)
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
urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Arizona%20EORP.csv"
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
reason.data2[plan_name == "CalPERS - California Public Employees Retirement Fund" & year > 2016]$arr <-c(0.0715, 0.0715, 0.07)
reason.data2[plan_name == "Arkansas Public Employees System" & year == 2019]$return_1yr <-c(0.0532)
reason.data2[plan_name == "Arkansas Public Employees System" & year == 2019]$mva <-c(8833327660)
reason.data2[plan_name == "Arkansas Teachers Retirement Plan" & year == 2019]$return_1yr <-c(0.0519)
reason.data2[plan_name == "Arkansas Teachers Retirement Plan" & year == 2019]$mva <-c(17741621773)

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(plan.names)
for (i in 1:plan.names[,.N]){
  reason.data2[plan_name %in% plan.names[i,1]]$plan_name <- as.character(plan.names[i,2])
}
#write.csv(reason.data2, file = "/Users/anilniraula/Downloads/reason.data.2019.csv")
#colnames(reason.data2)
reason.data1.n19 <- data.table(reason.data2[year == 2019])
reason.data1.n19$year <- 2019

#Save plan names that miss aal or mva for 2019
names <- reason.data2[year == 2019 & is.na(mva) | year == 2019 & is.na(aal)]$plan_name
#reason.data1.n20[plan_name == names[2]]$aal
reason.data2[year == 2018 & is.na(payroll_growth_assumption)]$payroll_growth_assumption <- 0.03

#Function to search last non-zero value
gl <- function(x) {
  last(na.omit(x))
}

###2019 projections
for (i in 1:length(names)){
  reason.data1.n19[plan_name == names[i]]$aal <-  
    (gl(reason.data2[plan_name == names[i]]$aal) *(1 + gl(reason.data2[plan_name == names[i]]$arr)) 
     + (gl(reason.data2[plan_name == names[i]]$total_nc_pct)*gl(reason.data2[plan_name == names[i]]$payroll) +
          gl(reason.data2[plan_name == names[i]]$total_benefit_payments))*
       (1 + gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption))*
       (1 + gl(reason.data2[plan_name == names[i]]$arr)) ^ 0.5)
  reason.data1.n19[plan_name == names[i]]$mva <- 
    (gl(reason.data2[plan_name == names[i]]$mva)*(1 + return2019) 
     + (gl(reason.data2[plan_name == names[i]]$total_proj_adec_pct)*gl(reason.data2[plan_name == names[i]]$payroll) +
          gl(reason.data2[plan_name == names[i]]$total_benefit_payments))*
       (1 + gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption)) * (1 + return2019) ^ 0.5)
  
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
  reason.data1.n19[plan_name == names[i]]$return_1yr <- return2019
  reason.data1.n19[plan_name == names[i]]$total_nc_pct <- 
    (gl(reason.data2[plan_name == names[i]]$total_nc_pct))
  reason.data1.n19[plan_name == names[i]]$total_proj_adec_pct <- 
    (gl(reason.data2[plan_name == names[i]]$total_proj_adec_pct))
  reason.data1.n19[plan_name == names[i]]$payroll_growth_assumption <- 
    (gl(reason.data2[plan_name == names[i]]$payroll_growth_assumption))
}

#Add data with 2019 projections
reason.data2 <- data.table(bind_rows(reason.data2[year < 2019], reason.data1.n19) %>% arrange(plan_name))
#Assign paroll growth assumption
#View(reason.data1.n19)
table(is.na(reason.data2[year == 2019]$mva))#any more missing values for 2019?
#write.xlsx(PPD2.3, file = "/Users/anilniraula/Downloads/PPD_Avgpayroll.xlsx")
reason.data2 <- data.table(reason.data2)

reason.data2$total_proj_adec_pct <- as.numeric(reason.data2$total_proj_adec_pct)
reason.data2$aal <- as.numeric(reason.data2$aal)
reason.data2$mva <- as.numeric(reason.data2$mva)
reason.data2$total_nc_pct <- as.numeric(reason.data2$total_nc_pct)
reason.data2$payroll <- as.numeric(reason.data2$payroll)
reason.data2$payroll_growth_assumption <- as.numeric(reason.data2$payroll_growth_assumption)
reason.data2$arr <- as.numeric(reason.data2$arr)
reason.data2$return_1yr <- as.numeric(reason.data2$return_1yr)
reason.data2$total_benefit_payments <- as.numeric(reason.data2$total_benefit_payments)
reason.data2$year <- as.numeric(reason.data2$year)

reason.data2 <- reason.data2[year > 2000]
reason.data1.n20 <- reason.data2[plan_name == names[1]]
#View(reason.data2.n20)
#View(reason.data1.n20)
reason.data1.n20$aal <- reason.data2[, sum(na.omit(aal)), by=list(year)]$V1
reason.data1.n20$mva <- reason.data2[, sum(na.omit(mva)), by=list(year)]$V1
reason.data1.n20$payroll <- reason.data2[, sum(na.omit(payroll)), by=list(year)]$V1
reason.data1.n20$payroll_growth_assumption <- reason.data2[, median(na.omit(payroll_growth_assumption)), by=list(year)]$V1
reason.data1.n20$total_benefit_payments <- reason.data2[, sum(na.omit(total_benefit_payments)), by=list(year)]$V1
reason.data1.n20$total_nc_pct <- reason.data2[, median(na.omit(total_nc_pct)), by=list(year)]$V1
reason.data1.n20$total_proj_adec_pct <- reason.data2[, median(na.omit(total_proj_adec_pct)), by=list(year)]$V1
reason.data1.n20$arr <- reason.data2[, median(na.omit(arr)), by=list(year)]$V1
reason.data1.n20$return_1yr <- reason.data2[, median(na.omit(return_1yr)), by=list(year)]$V1

reason.data2.n20 <- data.table(reason.data1.n20[year == 2019])
reason.data2.n20$year <- 2020
#View(reason.data2[state == "Alabama"])

###Shiny App###
###############
###Interface###
###############
ui <- fluidPage(
  #Mobile friendly:
  #HTML('<meta name="viewport" content="width=1024">'),
  titlePanel("State Pension Challenges – Unfunded Liabilities Before and After COVID-19"),
  # CODE BELOW: Add select input named "PlanName" to choose between different pension plans in PPD database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(
      img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
      br(),
      br(),
      selectInput("x", "Choose State", choices = c(unique(reason.data2$state)), selected = c("Alabama")),
      #aking first choice empty - setting choices = c("", unique(PPD)...)
      #ADD slider input to choose year range
      uiOutput("secondSelection"),
      sliderInput("return2020", "Choose the Investment Return for Fiscal Year 2019-2020:",
                  min = -25, max = 10, post = " %", value = -7, step = 1),
      #textInput('filename', "Filename"),
      #checkboxInput('savePlot', "Check to save"),
      em("-Hover over the chart for additional information."),
      br(),
      em("-Go to 'U.S.' tab for an aggregate pension plan analysis."),# "\n", "(modified with investment return)."),  
      tags$div(em(HTML(paste0("-For more public pension analysis, visit ", 
                              tags$a(href="https://reason.org/topics/pension-reform/", em("reason.org/pensions")))))),  
      #br(),
      #em("-You can 'Download plot as a png' in the top-right corner."),
    ),
    # Button
    #downloadButton("downloadData", "Download")
    #),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel("Plan-by-Plan",plotly::plotlyOutput("plot_UAL")),
        tabPanel("U.S.",plotly::plotlyOutput("plot_UAL2")#,
                 #downloadButton('downloadpliot1', 'Download graph as pdf')
        ),
        
        tags$div(htmlOutput("text1")),
        tags$div(HTML(paste("<br>"))),
        tags$div(htmlOutput("text2")),
        tags$div(HTML(paste("<br>"))),
        tags$div(htmlOutput("text3"))
      )
    )
  )
)

##SERVER##
server <- function(input, output, session){
  #https://reasonfoundation.github.io/R-sandbox/
  
  output$secondSelection <- renderUI({
    pl1 <- reason.data2[state == input$x]
    selectInput("y", "Choose State Pension Plan", choices = unique(pl1$plan_name))
  })
  
  #Add notifications
  #observe ({
  #  showNotification(
  #    paste("You have selected", input$y, "and", input$return2020, "2020 return")
  #  )
  #})
  
  output$text1 <- renderText({
    paste(HTML("<br>",
               "<b>Source</b>:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"), 
          "analysis of U.S. public pension actuarial valuation reports and CAFRs.", "<br>", 
          "Dotted lines for 2019/2020 fiscal years show Reason projections.", sep="\n")
  })
  
  output$text2 <- renderText({
    paste(HTML(
      "<b>Notes</b>: All figures presented are based on the market value of assets (MVA) - the real value of plan’s total assets","\n", 
      "measured by the price of selling all assets in an orderly transaction at that date.","\n", 
      "Because some plans have not released their 2019 reports,","\n",
      "part of the 2019 data was estimated. These plans are assumed to have earned the average return earned by the rest of their peers in 2019, which was 6.2 percent.","\n",
      "Plans with no data on payroll growth assumption were assigned an average payroll growth assumed by other plans in 2018, which was 3.0 percent.", sep="\n"))
  })
  
  output$text3 <- renderText({
    paste(HTML("<b>Methodology</b>: Each plan's actuarial accrued liability (total value of the pension benefits", "\n",  
               "promised to employees to date) is rolled forward one year by adding the normal cost and subtracting the expected benefit payment,", 
               "adjusted by the assumed rate of return (discount rate). The MVA is projected by adding the required contribution and subtracting the expected benefit payment,", 
               "adjusted by the user-selected investment return for fiscal year 2020.",
               sep="\n"))
  })
  
  #Create a reactive PPD data with 2020 projections to call later for visualization
  ReasonData <- reactive ({
    
    reason.data1.n20 <- reason.data2[year == 2019 & plan_name == input$y]
    reason.data2.20 <- reason.data2[plan_name == input$y]
    reason.data1.n20$year <- 2020
    
    for (i in 1:length(names)){
      reason.data1.n20$aal <-  
        (gl(reason.data2.20$aal) *(1 + gl(reason.data2.20$arr)) 
         + (gl(reason.data2.20$total_nc_pct)*gl(reason.data2.20$payroll) +
              gl(reason.data2.20$total_benefit_payments))*
           (1 + gl(reason.data2.20$payroll_growth_assumption))*
           (1 + gl(reason.data2.20$arr)) ^ 0.5)
      
      reason.data1.n20$mva <- 
        (gl(reason.data2.20$mva)*(1 + input$return2020/100) 
         + (gl(reason.data2.20$total_proj_adec_pct)*gl(reason.data2.20$payroll) +
              gl(reason.data2.20$total_benefit_payments))*
           (1 + gl(reason.data2.20$payroll_growth_assumption)) * (1 + input$return2020/100) ^ 0.5)
      
      reason.data1.n20$payroll <- 
        (gl(reason.data2.20$payroll)*
           (1 + gl(reason.data2.20$payroll_growth_assumption)))
      
      reason.data1.n20$total_benefit_payments <- 
        (gl(reason.data2.20$total_benefit_payments)*
           (1 + gl(reason.data2.20$payroll_growth_assumption)))
      
      reason.data1.n20$state <- c(gl(reason.data2.20$state))
      reason.data1.n20$arr <- c(gl(reason.data2.20$arr))
      reason.data1.n20$return_1yr <- input$return2020/100
      reason.data1.n20$total_nc_pct <- 
        (gl(reason.data2.20$total_nc_pct))
      reason.data1.n20$total_proj_adec_pct <- 
        (gl(reason.data2.20$total_proj_adec_pct))
      reason.data1.n20$payroll_growth_assumption <- 
        (gl(reason.data2.20$payroll_growth_assumption))
    }
    
    #Add 2020 estimates to the previous data set
    reason.data3 <- data.table(bind_rows(reason.data2.20, reason.data1.n20) %>% arrange(plan_name))
    #Add a column to differentiate actual & estimated numbers
    #write.xlsx(Preason.data3, file = "/Users/anilniraula/Downloads/reason.data.2020.xlsx")
    reason.data3 <- data.table(reason.data3[year >= 2001])
    reason.data3[, Act_Est := ifelse((plan_name %in% names & year == 2018 | year > 2018),"Estimate","Real")]
    reason.data3
    #View(reason.data3)
  })
  
  ReasonData2 <- reactive ({
    
    reason.data1.n20 <- reason.data2[plan_name == names[1]]
    #View(reason.data2.n20)
    #View(reason.data1.n20)
    reason.data1.n20$aal <- reason.data2[, sum(na.omit(aal)), by=list(year)]$V1
    reason.data1.n20$mva <- reason.data2[, sum(na.omit(mva)), by=list(year)]$V1
    reason.data1.n20$payroll <- reason.data2[, sum(na.omit(payroll)), by=list(year)]$V1
    reason.data1.n20$payroll_growth_assumption <- reason.data2[, median(na.omit(payroll_growth_assumption)), by=list(year)]$V1
    reason.data1.n20$total_benefit_payments <- reason.data2[, sum(na.omit(total_benefit_payments)), by=list(year)]$V1
    reason.data1.n20$total_nc_pct <- reason.data2[, median(na.omit(total_nc_pct)), by=list(year)]$V1
    reason.data1.n20$total_proj_adec_pct <- reason.data2[, median(na.omit(total_proj_adec_pct)), by=list(year)]$V1
    reason.data1.n20$arr <- reason.data2[, median(na.omit(arr)), by=list(year)]$V1
    reason.data1.n20$return_1yr <- reason.data2[, median(na.omit(return_1yr)), by=list(year)]$V1
    
    reason.data2.n20 <- data.table(reason.data1.n20[year == 2019])
    reason.data2.n20$year <- 2020
    #View(reason.data2.n20)
    #View(reason.data1.n20)
    
    reason.data2.n20$aal <-  
      (gl(reason.data1.n20$aal) *(1 + gl(reason.data1.n20$arr)) 
       + (gl(reason.data1.n20$total_nc_pct)*gl(reason.data1.n20$payroll) +
            gl(reason.data1.n20$total_benefit_payments))*
         (1 + gl(reason.data1.n20$payroll_growth_assumption))*
         (1 + gl(reason.data1.n20$arr)) ^ 0.5)
    
    reason.data2.n20$mva <- 
      (gl(reason.data1.n20$mva)*(1 + input$return2020/100) 
       + (gl(reason.data1.n20$total_proj_adec_pct)*gl(reason.data1.n20$payroll) +
            gl(reason.data1.n20$total_benefit_payments))*
         (1 + gl(reason.data1.n20$payroll_growth_assumption)) * (1 + input$return2020/100) ^ 0.5)
    
    reason.data2.n20$payroll <- 
      (gl(reason.data1.n20$payroll)*
         (1 + gl(reason.data1.n20$payroll_growth_assumption)))
    
    reason.data2.n20$total_benefit_payments <- 
      (gl(reason.data1.n20$total_benefit_payments)*
         (1 + gl(reason.data1.n20$payroll_growth_assumption)))
    
    reason.data2.n20$state <- c(gl(reason.data1.n20$state))
    reason.data2.n20$arr <- c(gl(reason.data1.n20$arr))
    reason.data2.n20$return_1yr <- input$return2020/100
    reason.data2.n20$total_nc_pct <- 
      (gl(reason.data1.n20$total_nc_pct))
    reason.data2.n20$total_proj_adec_pct <- 
      (gl(reason.data1.n20$total_proj_adec_pct))
    reason.data2.n20$payroll_growth_assumption <- 
      (gl(reason.data1.n20$payroll_growth_assumption))
    
    #View(reason.data2.n20)
    #Add 2020 estimates to the previous data set
    reason.data3 <- data.table(bind_rows(reason.data2.n20, reason.data1.n20) %>% arrange(plan_name, year))
    #Add a column to differentiate actual & estimated numbers
    #write.xlsx(Preason.data3, file = "/Users/anilniraula/Downloads/reason.data.2020.xlsx")
    reason.data3[, Act_Est := ifelse((year > 2018),"Estimate","Real")]
    reason.data3
  })
  
  output$plot_UAL <- plotly::renderPlotly({
    #Estimate 2020 AAL & MVA for all state plans using user 2020 return input
    ###########
    #Create data frame for visualization
    UAL <- data.table(ReasonData())
    #View(UAL)
    UAL <- UAL[plan_name == input$y]
    UAL <- data.table("Market_Assets"= as.numeric(UAL$mva)/1000000000, 
                      "Actuarial_Liability"= as.numeric(UAL$aal)/1000000000,
                      "Fiscal_Year"= as.numeric(UAL$year),# referenced filtered years instead of a fixed 2001-20 sequence,
                      "Act_Est" = ifelse(UAL$Act_Est == "Estimate",1,0),
                      "UAL_MVA" = as.numeric(UAL$aal-UAL$mva)/1000000000)
    
    
    UAL$Fiscal_Year <- as.numeric(UAL$Fiscal_Year)
    UAL <- data.frame(UAL)
    #View(UAL)
    #Separating out Real vs. Estimated values using conditional area/line graphs + 
    #modify hover legends using text()+tooltip()
    p <- ggplot() +
      ggtitle(label = paste0("Plan: ", input$y))+
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), color="Unfunded Liability"), fill = "gray88")+
      geom_area(data=UAL, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Actuarial_Liability,2), NA)), fill = "gray98", alpha = 0.4)+
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Market_Assets,2)), fill = "white")+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), 
                              color="Liability", group = 1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Liability: $",round(Actuarial_Liability,1),"B"
                              )), 
      )+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Actuarial_Liability,2), NA), 
                              color="Liability", group = 1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Liability: $",ifelse(Act_Est == 1,round(Actuarial_Liability,1), NA),"B"
                              )), linetype = "dotted", color = "white")+
      ##Adding Points
      geom_point(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), 
                               color="Liability", group = 1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Liability: $",round(Actuarial_Liability,1),"B"
                               )), size = 1.00)+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=round(Market_Assets,2), 
                              color="Assets", group = 1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Assets: $",round(Market_Assets,1), "B"
                              )), 
      )+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Market_Assets,2), NA), 
                              color="Assets", group = 1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>Assets: $",ifelse(Act_Est == 1,round(Market_Assets,1), NA), "B"
                              )), linetype = "dotted", color = "white")+
      ##Adding Points
      geom_point(data=UAL, aes(x=Fiscal_Year, y=round(Market_Assets,2), 
                               color="Assets", group = 1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Assets: $",round(Market_Assets,1), "B"
                               )), size = 1.00)+
      scale_colour_manual(values=c("orangered2", "royalblue3", "white", "white"))+
      #"#FF6633", "#0066CC",
      scale_y_continuous(labels = function(x) paste0("$",x,"B"), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(2001, 2020, by = 1), limits = c(2001, 2020))+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 8, colour = "white", face = "bold")
      )+
      
      theme(
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        axis.text.y = element_text(size=10, color = "black"),
        axis.text.x = element_text(size=10, color = "black", angle = 90, hjust = 1, vjust = 0.5),
      )+
      #Annotating ending UAL
      #Add annotating line
      annotate("text", fontface = 'bold', size = 3.3, x=2015, y=(UAL[15,1]/2),
               label = paste0("<B>FY</B>", "<B>",2020,"</B>","<B>:</B>", sep="\n", 
                              " <B>$</B>","<B>",round(last(UAL$UAL_MVA),1),"</B>", "<B>B</B>",
                              "<B> Pension Debt </B>", sep="\n", "<B>(projected with </B>", "<B>",
                              input$return2020,"</B>", "<B>% return)</B>"))
    
    p <- p + annotate("text", fontface = 'bold', size = 2.7, x=2017, y=0, size=3,
                      label = paste0("reason.org/pensions"))
    
    p <- ggplotly(p, tooltip = c("text"))
    
    p <- p %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    p = p %>%
      #https://mran.revolutionanalytics.com/snapshot/2016-03-14/web/packages/plotly/plotly.pdf
      config(staticPlot = F, 
             editable = F,
             autosizable = T,
             showTips = T,
             displayModeBar = T,#Removing the all the buttons
             modeBarButtonsToRemove = list(
               #"toImage",
               "zoom2d",
               "pan2d",
               "lasso2d",
               "lasszoom2d",
               "pan2d",
               "select2d",
               "zoomIn2d",
               "zoomOut2d",
               "autoScale2d",
               "resetScale2d",
               "hoverClosestCartesian",
               "hoverCompareCartesian",
               "sendDataToCloud",
               "toggleHover",
               "resetViews",
               "toggleSpikelines",
               "resetViewMapbox"
             ),
             displaylogo = FALSE
      ) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE))
    
    # output$downloadplot1 <- downloadHandler(
    #  filename ="plot.pdf",
    #    content = function(file) {
    #      pdf(file, width=12, height=6.3)
    #      print(testplot())
    #      dev.off()
    #    })
    
    
  })
  
  ###Aggregate graph
  output$plot_UAL2 <- plotly::renderPlotly({
    ###########
    
    #Create data frame for visualization (call reactive data with 2020 projections)
    UAL2 <- data.table(ReasonData2())
    UAL2 <- data.table("Market_Assets"= (UAL2$mva)/1000000000000, 
                       "Actuarial_Liability"= (UAL2$aal)/1000000000000,
                       "Fiscal_Year"= as.numeric(UAL2$year),# referenced filtered years instead of a fixed 2001-20 sequence,
                       "Act_Est" = ifelse(UAL2$year >= 2018,1,0),
                       "UAL_MVA" = (UAL2$aal-UAL2$mva)/1000000000000)
    UAL2 <- data.frame(UAL2[1:20])
    #View(UAL2)
    #Separating out Real vs. Estimated values using conditional area/line graphs + 
    #modify hover legends using text()+tooltip()
    u <- ggplot() +
      ggtitle(label = paste0("All Plans Combined"))+
      geom_area(data=UAL2, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), color="Unfunded Liability"), fill = "gray88")+
      geom_area(data=UAL2, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Actuarial_Liability,2), NA)), fill = "gray98", alpha = 0.4)+
      geom_area(data=UAL2, aes(x=Fiscal_Year, y=round(Market_Assets,2)), fill = "white")+
      geom_line(data=UAL2, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), 
                               color="Liability", group = 1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Liability: $",round(Actuarial_Liability,1),"T"
                               )), 
      )+
      geom_line(data=UAL2, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Actuarial_Liability,2), NA), 
                               color="Liability", group = 1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Liability: $",ifelse(Act_Est == 1,round(Actuarial_Liability,1), NA),"T"
                               )), linetype = "dotted", color = "white")+
      ##Adding Points
      geom_point(data=UAL2, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), 
                                color="Liability", group = 1,
                                text = paste0("Fiscal Year: ", Fiscal_Year,
                                              "<br>Liability: $",round(Actuarial_Liability,1),"T"
                                )),
                 size = 1.00)+
      geom_line(data=UAL2, aes(x=Fiscal_Year, y=round(Market_Assets,2), 
                               color="Assets", group = 1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Assets: $",round(Market_Assets,1), "T"
                               )), 
      )+
      geom_line(data=UAL2, aes(x=Fiscal_Year, y=ifelse(Act_Est == 1,round(Market_Assets,2), NA), 
                               color="Assets", group = 1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>Assets: $",ifelse(Act_Est == 1,round(Market_Assets,1), NA), "T"
                               )), 
                linetype = "dotted", color = "white")+
      ##Adding Points
      geom_point(data=UAL2, aes(x=Fiscal_Year, y=round(Market_Assets,2), 
                                color="Assets", group = 1,
                                text = paste0("Fiscal Year: ", Fiscal_Year,
                                              "<br>Assets: $",round(Market_Assets,1), "T"
                                )),  
                 size = 1.00)+
      
      scale_colour_manual(values=c("orangered2", "royalblue3", "white", "white"))+
      scale_y_continuous(labels = function(x) paste0("$",x,"T"), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                         breaks = seq(2001, 2020, by = 1), limits = c(2001, 2020))+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)
      )+
      theme(
        #  legend.position = c(.6, .85),
        #  legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        axis.text.y = element_text(size=10, color = "black"),
        axis.text.x = element_text(size=10, color = "black", angle = 90, hjust = 1, vjust = 0.5),
      )+
      
      theme(legend.title = element_text(size = 8, colour = "white", face = "bold"))+
      #Annotating ending UAL
      annotate("text", fontface = 'bold', size = 3.3, x=2015, y=(UAL2[15,1]/2),
               label = paste0("<B>FY</B>", "<B>",2020,"</B>","<B>:</B>", sep="\n", 
                              " <B>$</B>","<B>",round(last(UAL2$UAL_MVA),1),"</B>", "<B>T</B>",
                              "<B> Total Pension Debt </B>", sep="\n", "<B>(projected with </B>", "<B>",
                              input$return2020,"</B>", "<B>% return)</B>"))
    
    #label = paste0("FY", max(UAL2$Fiscal_Year),":", sep="\n", 
    #              " $",round(last(UAL2$UAL_MVA),1), "T",
    #               " Pension Debt", sep="\n", "(projected)"))
    
    u <- u + annotate("text", fontface = 'bold', size = 2.7, x=2017, y=0, size=3,
                      label = paste0("reason.org/pensions"))
    
    u <- ggplotly(u, tooltip = c("text"))
    u <- u %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    u = u %>%
      #https://mran.revolutionanalytics.com/snapshot/2016-03-14/web/packages/plotly/plotly.pdf
      config(staticPlot = F, 
             editable = F,
             autosizable = T,
             showTips = T,
             displayModeBar = T,#Removing the all the buttons
             modeBarButtonsToRemove = list(
               #"toImage",
               "zoom2d",
               "pan2d",
               "lasso2d",
               "lasszoom2d",
               "pan2d",
               "select2d",
               "zoomIn2d",
               "zoomOut2d",
               "autoScale2d",
               "resetScale2d",
               "hoverClosestCartesian",
               "hoverCompareCartesian",
               "sendDataToCloud",
               "toggleHover",
               "resetViews",
               "toggleSpikelines",
               "resetViewMapbox"
             ),
             displaylogo = FALSE
      ) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE))
    
    # output$downloadplot1 <- downloadHandler(
    #  filename ="plot.pdf",
    #    content = function(file) {
    #      pdf(file, width=12, height=6.3)
    #      print(testplot())
    #      dev.off()
    #    })
    
    u
    
  })
}

####Create app
shinyApp(ui = ui, server = server)
##########
#sink()
#Resources:
#manual color reference: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
#https://dereksonderegger.github.io/570L/9-graphing-using-ggplot2.html
#polygon(c(Year, rev(Year)), c(UAL[,1], rev(UAL[,2])),col = "grey30", border = NA)
#geom_ribbon(aes(ymin=UAL[,2],ymax=UAL[,1]), fill="blue", alpha="0.5"
