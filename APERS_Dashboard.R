### Arkansas PERS Dashboard ###
## Manual Data ##
#(by Anil Niraula)#

#https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
#****Create graph(s) tab showing Inv.Returns using BBC package: https://bbc.github.io/rcookbook/
rm(list=ls())
###Load/install packagesx
#R.Version()
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
#library(openxlsx)
library(tseries)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(ggplot2)
library(tidyverse)
#Shiny-----------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(htmlwidgets)
#library(shinyFiles)
library(DT)# -- for interactive tables (DT::renderDT() & DT::DTOutput())
library(plotly)
#devtools::install_github("ropensci/plotly")
library(plyr)
library(dplyr)

#########
pl <- planList() 
#View(arrange(pl, by = id))
states <- as.character(unique(pl[,3]))
plans <- as.character(unique(pl[,2]))

####################
#Loading our own APERS data
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_GL.csv"
APERS.gainloss <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
View(APERS.gainloss)
####################

#UAL2 <- data.frame(UAL2)
#https://github.com/ReasonFoundation/pensionviewr
###Columns were some plans have no data for
columns <- c("total_pension_liability_dollar", "wage_inflation",
             "payroll_growth_assumption", "other_contribution_dollar",
             "other_additions_dollar", "x1_year_investment_return_percentage",
             "fiscal_year_of_contribution", "statutory_payment_dollar",
             "statutory_payment_percentage",  "amortizaton_method",
             "number_of_years_remaining_on_amortization_schedule",
             "employer_normal_cost_percentage")

#Create a function 
#for filtering out sample of data variable we commonly use in pension analysis
filteredData <- function(data, plan, fy){
  Plan <- data.table(pullData(data, plan))
  for (i in (1:length(columns))){
    if(sum((colnames(Plan) == columns[i]))==0) {
      Plan[,columns[i] := NA] }
  }
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

palette_reason <- data.table(
  Orange = "#FF6633", 
  LightOrange = "#FF9164",
  DarkGrey = "#333333", 
  SpaceGrey = "#A69FA1",
  DarkBlue = "#1696d2",
  GreyBlue = "#6699CC", 
  Yellow = "#FFCC33", 
  LightBlue = "#3399CC", 
  SatBlue = "#3366CC", 
  Green = "#669900", 
  Red = "#CC0000")

#View(pallette_reason$Green)
######## Download the whole database   
#state.plans <- data.table(pullData(pl, pl$display_name)) %>% arrange(state)
#Set "New Jersey Consolidated Police & Fire Pension Fund" to state-level plan
#state.plans[display_name == "New Jersey Consolidated Police & Fire Pension Fund"]$administering_government_type <- 0 
#state.plans <- state.plans %>% filter(administering_government_type == 0)
#state.plans.nm <- unique(state.plans$display_name)
#View(state.plans)
#View(state.plans.nm)
#write.csv(state.plans, file = "/Users/anilniraula/Downloads/reason.data.state.csv")

#Load csv PPD file from GitHub repository
#urlfile="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/State.plan.names.csv"
#state.plan.names <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#state.plan.names <- data.table(state.plan.names)
##View(state.plan.names)

urlfile2="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/Reason_State_Names_Mod.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(plan.names)

# Load data ---------------------------------------------------------------
urlfile="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data <- data.table(reason.data %>% arrange(year))

#Load Reason database
#urlfile="https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/Reason_DataBase.csv"
#reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#reason.data <- read.csv("/Users/anilniraula/Downloads/Reason_DataBase_full.csv", stringsAsFactors=FALSE, blank.lines.skip = FALSE, header=T)
#reason.data <- data.table(reason.data)
#reason.data <- data.table(reason.data %>% filter(administering_government_type == 0))

### Run some Descriptive Statistics
#Modify datat format
#reason.data$x1_year_investment_return_percentage <- as.numeric(reason.data$x1_year_investment_return_percentage)
#reason.data$market_assets_reported_for_asset_smoothing <- as.numeric(reason.data$market_assets_reported_for_asset_smoothing)
#reason.data$market_value_of_assets_dollar <- as.numeric(reason.data$market_value_of_assets_dollar)
#MVA per year (summary)
#summary(reason.data[year == 2018]$market_value_of_assets_dollar/1000000)
#summary(reason.data[year == 2019]$market_value_of_assets_dollar/1000000)

#Missing vlues
#table(is.na(reason.data[year==2018]$market_value_of_assets_dollar))#How many plans missing mva in 2018 (#102 out of 114)
#View(reason.data[year==2018 & is.na(market_value_of_assets_dollar)]$display_name)#Plans with missing mva in 2018
#table(is.na(reason.data[year==2019]$market_value_of_assets_dollar))#How many plans missing mva in 2019 (same #102)
#table(is.na(reason.data[year==2019]$mva.ppd))#How many plans missing mva in 2019 in PPD (#0)
#View(reason.data[year==2019 & !is.na(market_value_of_assets_dollar)]$display_name)#Plans mva in 2019
#table(is.na(reason.data[year==2019]$market_assets_reported_for_asset_smoothing == 1))#How many plans missing mva in 2019
#View(reason.data[year==2018 & is.na(market_value_of_assets_dollar)]$display_name)#See plans missing mva in 2019

#mean(na.omit(reason.data[year == 2019]$x1_year_investment_return_percentage))
#mean(na.omit(reason.data$x1_year_investment_return_percentage), trim = 0.1)
#mean(na.omit(reason.data$market_assets_reported_for_asset_smoothing))
#median(na.omit(reason.data$x1_year_investment_return_percentage))
#Select state plans
#state.plans <- data.table(reason.data %>% filter(administering_government_type == 0))
#Filter & rename columns 
#View(reason.data$m)
reason.data <- reason.data %>%
  select(
    year,
    plan_name = display_name,
    state,
    return_1yr = x1_year_investment_return_percentage,
    actuarial_cost_method_in_gasb_reporting,
    funded_ratio = actuarial_funded_ratio_percentage,
    #actuarial_valuation_date_for_gasb_schedules,
    #actuarial_valuation_report_date,
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
    #other_contribution = other_contribution_dollar,#added
    #other_additions = other_additions_dollar,#added
    #fy = fiscal_year,
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
    #wage_inflation
  )

reason.data$mva <- as.numeric(reason.data$mva)
reason.data$return_yr <- as.numeric(reason.data$return_1yr)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$arr <- as.numeric(reason.data$arr)
reason.data$payroll <- as.numeric(reason.data$payroll)
reason.data$payroll_growth_assumption <- as.numeric(reason.data$payroll_growth_assumption)
reason.data$total_nc_pc <- as.numeric(reason.data$total_nc_pc)
reason.data$benefit_payments <- as.numeric(reason.data$benefit_payments)
reason.data$refunds <- as.numeric(reason.data$refunds)
reason.data$total_proj_adec_pct <- as.numeric(reason.data$total_proj_adec_pct)

perc.return <- matrix(0,19,5)
data <- reason.data[,quantile(na.omit(arr)), by=list(year)]$V1
#data
for (i in 1:19) {
  perc.return[i,] <- data[(i+(i-1)*4):(i*5)]
}
perc.return <- data.table(perc.return)
colnames(perc.return) <- c("5th", "25th", "50th", "75th", "95th")
#View(perc.return)

tr30 <- c(0.0688016, 0.067063095,	0.0660676,	0.055768,
          0.058722311,	0.059406375,	0.054948387,	0.0543,
          0.053007589,	0.051715179,	0.050422768,	0.049130357,
          0.048382869,	0.042775299,	0.0407664,	0.042510757,
          0.039108,	0.0292168,	0.0344616,	0.0333816,	0.02840996,
          0.025944,	0.0289404,	0.031115663,	0.0258)
#reason.data[, median(na.omit(return_1yr)), by=list(year)]$V1
#View(reason.data)
#Create Geomean function
#x <- c(0.1, 0.05, -0.04, NA)
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}

first.nan <- function(x) {
  first(x[which(!is.na(x))])
}
#geomean(x)

#Label state and local plans with*
#View(plan.names)
pl <- data.table(pl)
for (i in 1:plan.names[,.N]){
  pl[display_name %in% plan.names[i,1]]$display_name <- as.character(plan.names[i,2])
}

mva_us <- reason.data[, sum(na.omit(mva)), by=list(year)]/30000000000
mva_us <- mva_us[15:33]
mva_us$year <- seq(2001, 2019, by = 1)
mva_us <- data.table(mva_us)
#View(mva_us)

ual_us <- (reason.data[, sum(na.omit(aal)), by=list(year)]) %>% filter(year>2000)
x <- (reason.data[, sum(na.omit(mva)), by=list(year)]) %>% filter(year>2000)
ual_us$V1 <- (ual_us$V1 - x$V1)/1000000000000
#View(ual_us)
#pl[state == "Missouri"]$display_name
#View(pl)
#Create separate theme() for ggplot
plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0, 0,0,0, "cm"),
                               #plot.margin = margin(0.1, 3,0,3, "cm"),
                               axis.text.y = element_text(size=8, color = "black"),
                               axis.text.x = element_text(size=8, color = "black", angle = 90, hjust = 1, vjust = 0.5),
                               axis.title.y = element_text(size=9, color = "black"),
                               axis.title.x = element_text(size=9, color = "black"),
                               legend.title = element_text(size = 8, colour = "white", face = "bold"))

#https://stackoverflow.com/questions/26089617/subset-boxplots-by-date-order-x-axis-by-month

#ggplot() +
#  geom_density(data=UAL6, aes(x=Fiscal_Year, y=na.omit(Assumed_Return), color="ARR (U.S.)"),
#               fill = "royalblue3", size = 0.75)+
#  scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "",
#                     breaks = seq(0.05, 0.09, by = 0.01), limits = c(0.05, 0.09))+
#  scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
#                     breaks = seq(min(UAL6$Fiscal_Year), 2019, by = 1), limits = c(min(UAL6$Fiscal_Year), 2019))+
#  theme_bw()+
#  plotTheme#+


#reason.data2$mva <- reason.data2[, sum(na.omit(mva)), by=list(year)]$V1
#reason.data2$payroll <- reason.data2[, sum(na.omit(payroll)), by=list(year)]$V1
#reason.data2$payroll_growth_assumption <- reason.data2[, median(na.omit(payroll_growth_assumption)), by=list(year)]$V1
#reason.data2$total_benefit_payments <- reason.data2[, sum(na.omit(total_benefit_payments)), by=list(year)]$V1
#reason.data2$total_nc_pct <- reason.data2[, median(na.omit(total_nc_pct)), by=list(year)]$V1
#reason.data2$total_proj_adec_pct <- reason.data2[, median(na.omit(total_proj_adec_pct)), by=list(year)]$V1
#reason.data2$arr <- reason.data2[, median(na.omit(arr)), by=list(year)]$V1
#reason.data2$return_1yr <- reason.data2[, median(na.omit(return_1yr)), by=list(year)]$V1

reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)
funded <- reason.data[, median(na.omit(funded_ratio)), by=list(year)]
funded <- funded %>% filter(year >= 2001)
funded2 <- funded
funded <- funded$V1

#View(funded2)
#View(funded)
#View(reason.data2)
#View(reason.data2.n20)
#View(reason.data1.n20)

reason.data2 <- reason.data[plan_name == plan_name[1]]
reason.data2 <- reason.data[, sum(na.omit(aal)), by=list(year, state)]
reason.data3 <- reason.data[, sum(na.omit(mva)), by=list(year, state)]
reason.data4 <- reason.data[, median(na.omit(arr)), by=list(year, state)]
reason.data5 <- reason.data[, median(na.omit(mva))/20000000000, by=list(year, state)]
#View(reason.data2)
reason.data2 <- cbind(reason.data2, reason.data3$V1, reason.data4$V1, reason.data5$V1)
reason.data2 <- reason.data2 %>% arrange(state)
#colnames(reason.data2[,3]) <- c("aal")
#View(reason.data2)
reason.data2 <- data.table(reason.data2)
#set_reason_theme(style = "slide")
######Shiny App/Dashboard [interface]
##########################
help(glPlot)

#https://rstudio.github.io/shinydashboard/structure.html
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Pension Dashboard"),
                    dashboardSidebar(
                      title = "Control Panel",
                      sidebarMenu(
                        menuItem("APERS Dashboard", tabName = "main", icon = icon("dashboard")),
                        menuItem(sliderInput('year', 'Select Starting Year', min = 1990, max = 2019, value = 2001, sep = ""))
                      )
                    ),
                    dashboardBody(
                      ###Remove error messages
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"),
                      tabItems(
                        
                        # box(title = "Control Panel",
                        #    selectInput("x", "Select State", choices = states),
                        #    uiOutput("secondSelection"),
                        #    uiOutput("thirdSelection"),
                        #    sliderInput('year', 'Select Starting Year', min = 1990, max = 2019, value = 2001, sep = ""), 
                        #    height = 400, width = 3),
                        # infoBox("Plan's Funded Status is in Top:", paste(10, "%"), icon = icon("credit-card"), fill = TRUE),
                        
                        tabItem(tabName = "main",
                                tags$head(tags$style(HTML(".small-box {height: 100px}"))),#Height of calueBox
                                valueBoxOutput("top_ranking2"),#Edit to Show ending UAL + Add another 
                                valueBoxOutput("top_ranking"),
                                tabBox(
                                  side = "left",
                                  tabPanel(title = "APERS Assets vs. Liabilities", background = "blue", width = 6, plotly::plotlyOutput("plot_Filtered", height = 245)), 
                                  tabPanel(title = "APERS Unfunded Liability", background = "blue", width = 6, plotly::plotlyOutput("plot_Filtered_UAL", height = 245))),
                                #height = 320, width = 7),
                                
                                box(title = "APERS vs. U.S. Funded Status", width = 6, background = "blue", plotly::plotlyOutput("plot_Funded", height = 250)), 
                                box(title = "APERS Investment Returns", width = 6,  background = "blue", plotly::plotlyOutput("plot_Filtered_Returns", height = 250)), 
                                box(title = "APERS Cash Flow Analysis", width = 6, background = "blue", plotly::plotlyOutput("plot_Filtered_CashFlow",height = 250)))
                        
                      )
                    )
)

######Shiny app[server]
##########################

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
  
  #https://reasonfoundation.github.io/R-sandbox/
  
  PlanData <- reactive({
    UAL <- data.table(filteredData(pl, "Arkansas Public Employees System (APERS)*", input$year))
  })
  
  output$plot_DataPull <- DT::renderDT({
    ###Specify data to show (Filter out variables)
    PlanData() 
  })
  
  output$plot_US <- plotly::renderPlotly({
    
    UAL10 <- data.table("Actuarial_Liability"= reason.data[, sum(na.omit(aal)), by=list(year, state)], 
                        "Funded_Ratio"= (reason.data[, sum(na.omit(mva)), by=list(year, state)]$V1/1000000000)/
                          (reason.data[, sum(na.omit(aal)), by=list(year, state)]$V1/1000000000),
                        "Assumed_Return"= reason.data[, median(na.omit(arr)), by=list(year, state)]$V1,
                        "Median_Assets" = reason.data[, median(na.omit(mva))/10000000000, by=list(year, state)]$V1)
    UAL <- data.table(UAL10 %>% 
                        arrange(Actuarial_Liability.state) %>% 
                        filter(Actuarial_Liability.year == input$yearz))
    point <- data.table(UAL%>% 
                          filter(Actuarial_Liability.state==input$x))
    UAL <- data.frame(UAL)
    point <- data.frame(point)
    #View(UAL)
    #View(point)
    #colnames(reason.data2[,3]) <- c("aal")
    #View(reason.data2)
    p <- ggplot()+
      geom_point(data=UAL, aes(x = Funded_Ratio, y = Assumed_Return,
                               group =1,
                               text = paste0("State: ", Actuarial_Liability.state,
                                             "<br>Fiscal Year: ", input$yearz,
                                             "<br>Total Funded Ratio: ",round(Funded_Ratio,1)*100, "%",
                                             "<br>Median ARR: $",round(Assumed_Return*100,1), "%")),
                 color = palette_reason$GreyBlue, size = 2*(UAL$Funded_Ratio))+
      geom_point(data=point, aes(x = Funded_Ratio, y = Assumed_Return),
                 color= palette_reason$Orange, size = 3*(point$Funded_Ratio))+
      theme_bw()+
      scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "Median ARR (By State)",
                         breaks = seq(0.06, 0.09, by = 0.005), limits = c(0.06, 0.09))+
      scale_x_continuous(labels = function(x) paste0(x*100,"%"), name = "Funded Ratio (by State)",
                         breaks = seq(0.2, 1.6, by = 0.1), limits = c(0.2, 1.6))+
      plotTheme
    p <- ggplotly(p, tooltip = c("text"))
    p
  })
  
  output$plot_2019Updates <- renderText({
    Updt.2019 <- data.table(pullData(pl, input$y))
    Updt.2019 <-  min(max(Updt.2019$year), max(Updt.2019[!is.na(display_name)]$year))
    paste0("*Latest: ", Updt.2019, "FY", sep = "")
  })
  
  #Download the full database
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
      #ggtitle(label = paste0("Plan: ", input$y))+
      
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability,2), color="Unfunded Actuarial Liability"), fill = "gray88", size = 0.75)+
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Assets,2)), fill = "white", size = 0.75)+
      
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Actuarial_Liability, 
                              color="Actuarial Accrued Liability", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>AAL: $",round(Actuarial_Liability,1), "B")),
                size = 1.00)+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Actuarial_Assets, 
                              color="Actuarial Value of Assets", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>AVA: $",round(Actuarial_Assets,1), "B")),
                size = 1.00)+
      #manual color reference: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
      scale_colour_manual(values=c(palette_reason$SatBlue, palette_reason$Orange, "white", "white"))+
      scale_y_continuous(labels = function(x) paste0("$",x,"B"), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    #Annotating ending UAL
    annotate("text", fontface = 'bold', size = 3.5, x=2016, y=as.numeric((UAL[16,1]/2)), size=3,
             label = paste0("FY", last(UAL$Fiscal_Year),":", sep="\n", 
                            " $",round(last((UAL$UAL_AVA)),1), "B",
                            " Pension Debt", sep="\n"))
    
    p <- ggplotly(p, tooltip = c("text"))
    p <- p %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    p
  })
  
  output$top_ranking <- renderValueBox({
    UAL <- data.table(PlanData())
    UAL <- data.table("Funded_Ratio"= (UAL$funded_ratio), 
                      "Funded_Ratio_US" = funded,
                      "Fiscal_Year"= (UAL$year))
    
    valueBox(value = tags$p(ifelse(median(na.omit(UAL$Funded_Ratio))>median(na.omit(UAL$Funded_Ratio_US)), "Funded Above US Median*",  "Funded Below US Median*"), style = "font-size: 80%;"), HTML(paste0(
      "Plan Median Funded: ", round(median(na.omit(UAL$Funded_Ratio))*100,2), "%","<br>",
      "US Median Funded: ", 
      round(median(na.omit(UAL$Funded_Ratio_US))*100,2), "%",sep="<br>")), 
      icon = icon("exclamation-triangle"), color = "orange")
    
  })
  
  output$top_ranking2 <- renderValueBox({
    
    UAL <- data.table(PlanData())
    UAL <- data.table("Actuarial_Assets"= (UAL$ava)/1000000000, 
                      "Actuarial_Liability"= (UAL$aal)/1000000000,
                      "Fiscal_Year"= as.numeric(UAL$year),# referenced filtered years instead of a fixed 2001-20 sequence,
                      "UAL_AVA" = ((UAL$aal)-(UAL$ava))/1000000000,
                      "UAL_AVA2" = ifelse((UAL$aal<UAL$ava),1,0)
    )
    
    #View(UAL)
    valueBox(value = tags$p("Unfunded Liability*", style = "font-size: 80%;"), HTML(paste0(
      "$", round(last(na.omit(UAL$UAL_AVA)),2), 
      " Billion (Actuarial Value)", "<br>", "Fiscal Year: ", last(UAL[!is.na(UAL_AVA)]$Fiscal_Year),sep="<br>")), 
      icon = icon("gear"), color = "green")
    
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
  
  
  output$plot_Funded <- plotly::renderPlotly({
    UAL <- data.table(PlanData())
    UAL <- data.table("Funded_Ratio"= (UAL$funded_ratio), 
                      "Funded_Ratio_US" = funded,
                      "Fiscal_Year"= (UAL$year)
    )
    
    #View(UAL)
    UAL$Fiscal_Year <-as.numeric(UAL$Fiscal_Year)
    UAL <- data.frame(UAL)
    
    u <- ggplot() +
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Funded_Ratio, 
                              color="Funded Status", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year, input$y,
                                            " <br>Funded Status: ",round(Funded_Ratio,2)*100, "%")),
                fill = "orangered2", size = 1
      )+
      
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Funded_Ratio_US, 
                              color="Funded Status US", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            " <br>Funded US Status: ",round(Funded_Ratio_US,2)*100, "%")),
                fill = "royalblue", size = 1
      )+
      scale_colour_manual(values=c(palette_reason$Orange, palette_reason$SatBlue))+
      
      scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "",
                         breaks = seq(0.2, max((max(na.omit(UAL$Funded_Ratio))+0.1), 1.2), by = 0.1), limits = c(0.2,max((max(na.omit(UAL$Funded_Ratio))+0.1), 1.2)))+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    u <- ggplotly(u, tooltip = c("text"))
    u <- u %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.1))
    u
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
                fill = "orangered2"#, alpha = 1, size = 1
      )+
      
      # geom_area(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,round(UAL_AVA,2), NA)), fill = "white", alpha = 1)+
      # geom_area(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,round(UAL_AVA,2), NA)), fill = "green3", alpha = 1, size = 1)+  
      #  geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,0, NA)), color = "green3", size = 0.1)+
      #  geom_line(data=UAL, aes(x=Fiscal_Year, y=ifelse(UAL_AVA2==1,round(UAL_AVA,2), NA)), color = "green3", size = 0.1)+
      
      scale_colour_manual(values=c("orangered2"))+
      
      scale_y_continuous(labels = function(x) paste0("$",x,"B"), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    u <- ggplotly(u, tooltip = c("text"))
    u <- u %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    u
  })
  
  output$rolling_average <- reactive({
    UAL3 <- data.table(PlanData()) 
    #View(state.plans[plan_name == "Alabama Employees' Retirement System (ERS)"]$return_1y) 
    returns <- as.numeric(UAL3$return_1yr)
    nyear <- 10
    rolling <- geomean(returns[1:nyear])
  })
  
  output$plot_Filtered_Returns <- plotly::renderPlotly({
    
    UAL3 <- data.table(PlanData()) 
    
    #View(state.plans[plan_name == "Alabama Employees' Retirement System (ERS)"]$return_1y) 
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
      #ggtitle(label = paste0("Plan: ", input$y))+
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
      scale_colour_manual(values=c(palette_reason$Orange, palette_reason$SatBlue, "grey80"))+
      scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "",
                         breaks = seq(-0.28, 0.26, by = 0.04), limits = c(-0.28, 0.26))+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL4$Fiscal_Year), 2019, by = 1), limits = c(min(UAL4$Fiscal_Year), 2019))+
      theme_bw()+
      plotTheme#+
    
    k <- ggplotly(k, tooltip = c("text"))
    k <- k %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 0.01))
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
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ADEC,
                              color="ADEC Not Paid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC Not Paid: $",round(ADEC/1000000,2), " $Millions")), width = 0.7, 
               fill = "orangered1"
      )+
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ifelse((ADEC_Paid>ADEC), ADEC_Paid,0),
                              color="ADEC OverPaid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC OverPaid: $",round((ADEC_Paid-ADEC)/1000000,2), 
                                            " $Millions")), width = 0.7, 
               fill = "green3"
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
      
      scale_colour_manual(values=c(palette_reason$Yellow, "white", "white", "white"))+
      scale_y_continuous(labels = function(x) paste0("$", round((x/1000000),2), "M"), name = "Employer Contributions vs. ADEC",
      )+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(input$year-1, (last(UAL5$Fiscal_Year)+1), by = 1), limits = c(input$year-1, (max(UAL5$Fiscal_Year)+1)))+
      theme_bw()+
      ###Adding custom theme
      #https://departmentfortransport.github.io/R-cookbook/plots.html
      plotTheme#+
    
    c <- ggplotly(c, tooltip = c("text"))
    c <- c %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    c
  })
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
################################