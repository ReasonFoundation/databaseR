###Funded Ratio Change (by State)###
##by Anil Niraula##
#Data: Reason
#Type: Dumbbell

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
library(shinydashboard)
#library(shinyFiles)
library(DT)
library(plotly)

#### Load/Filter #####
pl <- planList() 
#View(arrange(pl, by = id))
states <- as.character(unique(pl[,3]))
plans <- as.character(unique(pl[,2]))

##
# Load data ---------------------------------------------------------------

reason.data <- pullStateData(2001)
reason.data <- as.data.table(reason.data %>% arrange(year))


#Custom Function to filter for number of variables we commonly use in pension analysis (state plans*)
filteredData <- function(Data,fy){
  columns <- c("total_pension_liability_dollar", "wage_inflation",
               "payroll_growth_assumption", "other_contribution_dollar",
               "other_additions_dollar", "x1_year_investment_return_percentage",
               "amortization_method", "number_of_years_remaining_on_amortization_schedule",
               "fiscal_year_of_contribution", "statutory_payment_dollar",
               "statutory_payment_percentage", "discount_rate_assumption", 
               "multiple_discount_rates", "asset_valuation_method_for_gasb_reporting",
               "cost_structure", "employer_normal_cost_percentage",
               "inflation_rate_assumption_for_gasb_reporting", "total_number_of_members",
               "total_projected_actuarial_required_contribution_percentage_of_payroll")
  
  Plan <- data.table(Data)
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
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
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

reason.data <- filteredData(reason.data, 2001)
#Manually imputed data for NC (2001), Arkasnas/Colorado/Alabama (2019)
reason.data[state == "North Carolina" & year == 2001]$aal <- c(9967547769, 35248769986)
reason.data[state == "Arkansas" & year == 2019]$mva <- c(8803211537, 17741621773)
reason.data[state == "Arkansas" & year == 2019]$aal <- c(11129000000, 21708867019)
reason.data[state == "Colorado" & year == 2019]$mva <- c(4545960000, 26936490000, 15819843000)
reason.data[state == "Colorado" & year == 2019]$aal <- c(5316433000, 42425061000, 25717648000)
reason.data[state == "Alabama" & year == 2019]$mva <- c(12568473000, 25619448000)
reason.data[state == "Alabama" & year == 2019]$aal <- c(18543542000, 37215470000)

#View(reason.data[state == "North Carolina" & year == 2001])
#View(reason.data[state == "Colorado" & year > 2017])

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(plan.names)

for (i in 1:plan.names[,.N]){
  reason.data[plan_name %in% plan.names[i,1]]$plan_name <- as.character(plan.names[i,2])
}

#View(reason.data[state == "Colorado"])

columns <- c("total_pension_liability_dollar", "wage_inflation",
             "payroll_growth_assumption", "other_contribution_dollar",
             "other_additions_dollar", "x1_year_investment_return_percentage",
             "fiscal_year_of_contribution", "statutory_payment_dollar",
             "statutory_payment_percentage")

filteredData <- function(pl, y, fy1, fy2){
  Plan <- data.table(pullData(pl, y))
  ##Create missing columns for plans with no data for st 7 variable
  for (i in (1:length(columns))){
    if(sum((colnames(Plan) == columns[i]))==0) {
      Plan[,columns[i] := NA] }
  }
  ####
  Plan <- Plan %>%
    filter(year > fy1-1 & year < fy2+1)
  Plan <- Plan %>%
    select(
      year,
      plan_name = display_name,
      state,
      return_1yr = x1_year_investment_return_percentage,
      actuarial_cost_method_in_gasb_reporting,
      funded_ratio = actuarial_funded_ratio_percentage,
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
      mva_smooth = market_assets_reported_for_asset_smoothing,#added
      aal = actuarially_accrued_liabilities_dollar,
      tpl = total_pension_liability_dollar
    )
}

reason.data <- data.table(reason.data)
reason.data <- data.table(reason.data %>%
                            select(
                              year, plan_name, state, return_1yr,
                              aal, mva, arr, 
                              payroll, payroll_growth_assumption, total_nc_pct,
                              total_proj_adec_pct,  total_benefit_payments))

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Arizona%20EORP.csv"
ArizonaEORP <-data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
ArizonaEORP <- ArizonaEORP[!20:25,]
ArizonaEORP <- data.table(ArizonaEORP %>%
               select(
               year = fy, plan_name = PlanName, state = StateName, return_1yr = InvestmentReturn_1yr,
               aal = ActLiabilities_GASB, mva = MktAssets_net, arr = InvestmentReturnAssumption_GASB, 
               payroll = payroll, payroll_growth_assumption = PayrollGrowthAssumption, total_nc_pct=NormCostRate_tot,
               total_proj_adec_pct=ReqContRate_tot,  total_benefit_payments = expense_TotBenefits))

reason.data <- data.table(rbind(reason.data, ArizonaEORP, fill=TRUE))
#View(reason.data[year == 2019])
#View(ArizonaEORP)
#View(reason.data[state == "Arizona"] %>% arrange(plan_name))

reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)
reason.data$mva <- as.numeric(reason.data$mva)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data2 <- data.table(reason.data[, sum(na.omit(mva)), by=list(year, state)])
reason.data2[,funded := reason.data[, sum(na.omit(mva)), by=list(year, state)]$V1/
               reason.data[, sum(na.omit(aal)), by=list(year, state)]$V1]
reason.data2 <- data.table(reason.data2)
reason.data$funded <- as.numeric(reason.data$funded)
reason.data2 <- data.table(reason.data2[year == 2001 | year == 2019] %>% arrange(state))
reason.data3 <- reason.data2 %>% select(state, year, funded)
reason.data.wide <- data.table(reason.data3 %>% pivot_wider(names_from = year, values_from = funded))
#http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
reason.data.wide[,gap := reason.data.wide[,2]-reason.data.wide[,3]]

#View(reason.data.wide)
DF <- data.table(reason.data.wide)
#View(ArizonaEORP)
#DF<- data.table(rbind(DF, ArizonaEORP, fill=TRUE))

DF <- data.table(DF[!state == "Montana" & colnames(DF[,2]) == 2001])
DF <- data.table(DF[!state == "Nebraska" & colnames(DF[,2]) == 2001])

#View(reason.data[state == "Arkansas" & year == 2019])
#View(DF)
#breaks <- seq(min(na.omit(DF$gap)), max(na.omit(DF$gap)), length = 5)
#freq <- cut(DF$gap, breaks = breaks, right = TRUE, include.lowest = TRUE)
#x <- data.table(table(freq))
#View(x)

#x$Freq <- seq (1, 4, by = 1)
#freq2 <- data.table(cbind(freq, DF$state))
#freq3 <- data.table(freq2 %>% arrange(freq))
#freq3[,range := ifelse((freq == 1), x$freq[1], ifelse((freq == 2), x$freq[2],                                              ifelse((freq == 4), ifelse((freq == 3), x$freq[3], x$freq[4]),x$freq[4])))]

#View(freq3 %>% arrange(freq))
#pl[state == "Missouri"]$display_name
#View(pl)
#Create separate theme() for ggplot
plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0, 0,0,0, "cm"),
                               #plot.margin = margin(0.1, 3,0,3, "cm"),
                               axis.text.y = element_text(size=8, color = "black"),
                               axis.text.x = element_text(size=8, color = "black", angle = 90, hjust = 1, vjust = 1),
                               axis.title.y = element_text(size=9, color = "black"),
                               axis.title.x = element_text(size=9, color = "black"),
                               legend.title = element_text(size = 5, colour = "white", face = "bold"))

#Or set_reason_theme(style = "slide")
#Custom color pallette that matches Reason Style

#### Palette ####
palette_reason <- data.table(
  Orange = "#FF6633", 
  LightOrange = "#FF9933",
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

#View(reason.data[state == "Arkansas" & year == 2019])
#https://rkabacoff.github.io/datavis/datavis.pdf

###SHINY DASHBOARD APP
ui <- dashboardPage(skin = "yellow",
      dashboardHeader(title = img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), 
                                                       width = 200, height = 45),
                      dropdownMenu(type  = "notifications", notificationItem(text = htmlOutput("text2"))
      )),
  
  #Change font of the body text
  #https://stackoverflow.com/questions/58454087/how-to-change-the-font-family-of-verbatimtextoutput-to-be-the-same-as-the-input
  dashboardSidebar(width = "270px",
      title = "",
      sidebarMenu(
      menuItem(sliderInput('x', h3('Select Time Period'), 
                           min = 2001, max = 2019, value = c(2001,2018), sep = "")),
      menuItem(uiOutput("secondSelection")),
     # em("Choose Starting and Ending Years to customize any period between 2001 and 2018"),
      menuItem((uiOutput("thirdSelection"))
      ))),
      dashboardBody(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
                 
      ),
     
      fluidRow(
        tags$head(tags$style(HTML(
          '.myClass { 
        font-size: 4px;
        line-height: 50px;
        text-align: left;
        font-family: "Arial",Arial;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
      box(
          width = 7, height = 670, background = "blue", plotly::plotlyOutput("plot_US"),
          tags$div(HTML(paste("<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>"))),
          tags$div(htmlOutput("text1")),
          #Specify Source line text font/size with css
          tags$head(tags$style("#text1{color: black;
                                 font-size: 14px;
                                 font-weight:bold;
                                 font-style: italic;
                                 }"
          )
          )),
      #Adjust size of the valueBox
      tags$head(tags$style(HTML(".small-box {height: 130px}"))),#
      valueBoxOutput("top_ranking"),
      box(width = 5, background = "blue",
          
          tags$head(
            tags$style(HTML("
            .selectize-input {
             font-size: 13pt;
             font-family: Arial;
             padding-top: 5px;
      }

    "))
       ),
          
          selectInput("s", "State Funded Status (Select State*)", choices = states),
          plotly::plotlyOutput("plot_State")))
      )
    )
##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  #Load abbriviated plan names
  Funded <- reactive({
    #Filter pl
    pl1 <- data.table(reason.data %>% filter(state == input$s))
    state.plans <-unique(pl1$plan_name)
    UAL <- data.table(reason.data[plan_name %in% state.plans[1]])
    if(length(state.plans)<2){UAL}else{
      for(i in (2:length(state.plans))){
        UAL <- rbind(UAL, reason.data[plan_name %in% state.plans[i]], fill=TRUE)
      }}
    UAL <- as.data.table(UAL)
    UAL <- UAL %>%
      filter(year >= input$x[1] & year <= input$x[2])
  })
  output$text1 <- renderText({
    paste(HTML(
               "<b>Source</b>:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"),"<br>", 
          "analysis of U.S. public pension actuarial valuation reports and CAFRs.", "<br>", 
          "Notes: Montana & Nebraska data for 2001 is incomplete. Some plans are yet to disclose 2019 data. ", sep="\n")
  })
  
  output$text2 <- renderText({
    paste(HTML(
      "<b>Methodology</b>", ": Funded Status = ", "<br>", 
      "Market Value of Assets divided by", "<br>", "Actuarial Accrued Liability. ", sep="\n"))
  })
    
  PlanData <- reactive({
    
  reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)
  reason.data$mva <- as.numeric(reason.data$mva)
  reason.data$aal <- as.numeric(reason.data$aal)
  reason.data$year <- as.numeric(reason.data$year)
  reason.data2 <- data.table(reason.data[, sum(na.omit(mva)), by=list(year, state)])
  reason.data2[,funded := reason.data[, sum(na.omit(mva)), by=list(year, state)]$V1/
                          reason.data[, sum(na.omit(aal)), by=list(year, state)]$V1]
  reason.data2 <- data.table(reason.data2)
  reason.data$funded <- as.numeric(reason.data$funded)
  reason.data2 <- data.table(reason.data2[year == input$x[1] | year == input$x[2]] %>% arrange(state))
  reason.data3 <- reason.data2 %>% select(state, year, funded)
  reason.data.wide <- data.table(reason.data3 %>% pivot_wider(names_from = year, values_from = funded))
  #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  reason.data.wide[,gap := reason.data.wide[,2]-reason.data.wide[,3]]
  
  DF <- data.frame(reason.data.wide)
  DF
  })
  
  output$thirdSelection <- renderUI({
      radioButtons("arrange", label = h3("Arrange States:"), 
                        choices = c("Alphabetical Order", "By Funded Status Change"),
                        selected = c("Alphabetical Order"))
  })
  
  output$plot_US <- plotly::renderPlotly({
  DF <- data.frame(PlanData())
  p <-  ggplot(DF, aes(x = ifelse((DF[,3]>DF[,2]), DF[,3], DF[,2]), xend = DF[,2], 
                       y = if(input$arrange == "Alphabetical Order"){
                         reorder(state, desc(state))
                         }else{
                         reorder(state, gap)},
                         group = state, fill = c("Ending Funded Status"),
                      xmin = 0, xmax = 1.25,
              text = paste0("State: ", state,
                            "<br>Period: ", input$x[1], "-", input$x[2], 
                            "<br>Funded Ratio Change: ", round(-DF[,4],2)*100, "%",
                            " (",round(DF[,2],2)*100,"%", " to ", round(DF[,3],2)*100,"%)")), 
              ) + 
    geom_col(aes(group = state, fill = c("Decline in Funded Status")),
      color = "white", size = 2)+
    geom_col(aes(x = DF[,3], xend = DF[,2],group = state, fill = c("Increase in Funded Status")), 
             color = "white", size = 2)+
    scale_fill_manual(values=c(palette_reason$LightBlue, palette_reason$Orange, palette_reason$Yellow, palette_reason$LightBlue, palette_reason$Red, palette_reason$DarkGrey))+
    geom_point(aes(x = DF[,3], xend = DF[,3], y = reorder(state, gap), group = state
                   ),col = palette_reason$Orange, size = 2) + 
    geom_point(aes(x = DF[,2], xend = DF[,2], y = reorder(state, gap), group = state, fill = c("Starting Funded Status")),
               col = palette_reason$LightBlue, size = 2) + 
    geom_col(aes(x = ifelse((DF[,3]>DF[,2]), DF[,2], DF[,3])-0.005, xend = DF[,3]-0.005, y = reorder(state, gap), group = state,
                 text = paste0("")),
              fill = "white", size = 2) +
    geom_vline(xintercept=(median(DF[,3]+median(DF[,4]))), text = paste0("Median US Funded Status"),
               linetype="dashed", 
               color = palette_reason$DarkGrey, size=1)+
    labs(caption = paste("reason.org/pensions"))+
    theme_bw()+
    labs(title= paste0("State Pension Funded Gap (",input$x[1], "-", input$x[2],")"),
         x = "Funded Ratio", y = "")+
    theme_bw()+
#  scale_y_continuous(labels = function(x) paste0(x), name = "")+
   scale_x_continuous(labels = function(x) paste0(x*100,"%"), name = "",
                     breaks = seq(0, 1.8, by = 0.2), limits = c(0, 1.8))+
   theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                      plot.margin = margin(0, 0,0,0, "cm"),
                      #plot.margin = margin(0.1, 3,0,3, "cm"),
                      axis.text.y = element_text(size=9, color = "black"),
                      axis.text.x = element_text(size=9, color = "black", angle = 90, hjust = 1, vjust = 1),
                      axis.title.y = element_text(size=9, color = "black"),
                      axis.title.x = element_text(size=9, color = "black"),
                      legend.title = element_text(size = 6, colour = "white", face = "bold"))
    
    p <- ggplotly(p, tooltip = c("text"))
    p <- p %>% layout(legend = list(orientation = "v", x=0.64, y = 0.5, font = list(size = 11)))
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
    p <- p %>% layout(autosize = T, height = 650)
    p
})


output$top_ranking <- renderValueBox({
  
  US.funded <- data.table(reason.data[year == input$x[1] | year == input$x[2]])
  x <- sum(na.omit(US.funded[year==input$x[1]]$mva))/sum(na.omit(US.funded[year==input$x[1]]$aal))
  y <- sum(na.omit(US.funded[year==input$x[2]]$mva))/sum(na.omit(US.funded[year==input$x[2]]$aal))
  US.funded <- y-x
  #View(US.funded)
  
  State.funded <- data.table(Funded())
  x <- data.table((as.numeric(sum(na.omit(State.funded[year==input$x[1]]$mva))))/
                    (as.numeric(sum(na.omit(State.funded[year==input$x[1]]$aal)))))
  y <- data.table((as.numeric(sum(na.omit(State.funded[year==input$x[2]]$mva))))/
                    (as.numeric(sum(na.omit(State.funded[year==input$x[2]]$aal)))))
  State.funded <- y-x
  #View(State.funded)
  
  valueBox("Funded Status", 
           HTML(paste0("Period: ", "<B>",input$x[1], "</B>","-", "<B>",input$x[2],"</B>",  
                "<br>","<B>","All US Plans: ","</B>", " Funded Status", ifelse(na.omit(US.funded)<0,paste0(" Declined "),
                paste0(" Increased ")),"<B>",round(na.omit(US.funded)*100,1), "%","</B>","<br>",
                "<B>", input$s, "</B>",": Funded Status", 
                ifelse(na.omit(State.funded)<0,paste0(" Declined "),
                paste0(" Increased ")), "<B>",
                ifelse(is.na(US.funded), paste("data not available"), round(na.omit(State.funded)*100,1)), "%","</B>",sep="<br>")), 
                icon = icon(ifelse(State.funded<0, paste0("arrow-down"), paste0("arrow-up"))),
                color = ifelse(State.funded<0, paste0("orange"), paste0("green")))
  
})

output$plot_State <- plotly::renderPlotly({
  UAL <- data.table(Funded())
  UAL <- data.table(UAL %>% select(plan_name, year, mva, aal))
  reason.data <- data.table(reason.data)
  EORP <- data.table(reason.data %>% 
      filter(plan_name == "Arizona EORP" & year >= input$x[1] & year <= input$x[2]))
      
  EORP <- data.table(EORP %>%select(plan_name, year, mva, aal))
  #View(EORP)
  #Adding Arizona EORP plan when user filter for Arizona state
  UAL <- data.table(if(sum(input$s == "Arizona")==1){rbind(UAL, EORP)}else{UAL})
  #View(UAL)
  UAL <- data.table("Pension_Plan" = UAL$plan_name,
                    "Funded_Status"= ((as.numeric(UAL$mva)/1000000000)/
                     (as.numeric(UAL$aal)/1000000000)),
                     "Fiscal_Year"= as.numeric(UAL$year))
  p <- ggplot() +
    ggtitle(label = paste0("State: ", input$s))+
    geom_line(data=UAL, aes(x=Fiscal_Year, y=Funded_Status, 
                            color=str_wrap(Pension_Plan,55), group =1,
                            text = paste0("Fiscal Year: ", Fiscal_Year,
                                          "<br>Funded Ratio: ",round(Funded_Status,2)*100, "%")),
              size = 1.00)+
    #theme(legend.key.height=unit(10, "cm"))+
    scale_colour_manual(values=c(palette_reason$SatBlue, palette_reason$Orange, palette_reason$Yellow, palette_reason$LightBlue, palette_reason$Red, palette_reason$Black))+
    scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "Funded Ratio (Market Value)",
                       breaks = seq(0, ifelse(max(na.omit(UAL$Funded_Status))<1.2,1.2,
                       ifelse(max(na.omit(UAL$Funded_Status))<1.4, 1.4, 1.6)), by = 0.2), 
                       limits = c(0, ifelse(max(na.omit(UAL$Funded_Status))<1.2,1.2,
                       ifelse(max(na.omit(UAL$Funded_Status))<1.4, 1.4, 1.6))))+
    scale_x_continuous(labels = function(x) paste0(x, ""), name = "Fiscal Year",
                       breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
    labs(caption = paste("reason.org/pensions"))+
    theme_bw()+
    plotTheme#+
  
  
  p <- ggplotly(p, tooltip = c("text"))
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
  
  p <- p %>% layout(autosize = T,
  legend = list(orientation = "v", x=0.01, y = 0.01, font = list(size = 10)))
  p
})
}

####Create app
shinyApp(ui = ui, server = server)
##########
#library("ggalt")
#library("gapminder")
#library("tidyr")

#Prepare data
#dumbbell_df <- gapminder %>%
#  filter(year == 1967 | year == 2007) %>%
#  select(country, year, lifeExp) %>%
#  spread(year, lifeExp) %>%
#  mutate(gap = `2007` - `1967`) %>%
#  arrange(desc(gap)) %>%
#  head(10)
#View(dumbbell_df)
#########