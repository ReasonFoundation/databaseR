#############################################
########## Arkansas PERS Dashboard ##########
#############################################

### Clear Ojects ###

rm(list = ls())


### Load Packages ###

library(pensionviewr)
library(tseries)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(plotly)
library(plyr)
library(dplyr)



### Reason Palette ###

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



### Arkansas Data ###

# Original
urlfile = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_numeric.csv"
APERS.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)


### US Data from PensionViewr ###

urlfile1 = "https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/Reason_State_Names_Mod.csv"
plan.names <- data.table(read_csv(url(urlfile1), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))

urlfile2 = "https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/reason.data.state.csv"
reason.data <- read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data <- data.table(reason.data %>% arrange(year))

pl <- planList()
states <- as.character(unique(pl[, 3]))
plans <- as.character(unique(pl[, 2]))



### Selecting Certain Columns ###

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



### Converting Certain Columns from Character to Numeric ###

char_cols <- c("mva",
               "return_1yr",
               "aal",
               "arr",
               "payroll",
               "payroll_growth_assumption",
               "total_nc_pct",
               "benefit_payments",
               "refunds",
               "total_proj_adec_pct")

reason.data[, (char_cols) := lapply(.SD, as.numeric), .SDcols = char_cols]


### Geometric Mean Function ###

geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}

first.nan <- function(x) {
  first(x[which(!is.na(x))])
}


### Label state and local plans with (*) asterisk ###

pl <- data.table(pl)
for (i in 1:plan.names[,.N]){
  pl[display_name %in% plan.names[i, 1]]$display_name <- as.character(plan.names[i,2])
}




ual_us <- (reason.data[, sum(na.omit(aal)), by=list(year)]) %>% filter(year > 2000)
x <- (reason.data[, sum(na.omit(mva)), by=list(year)]) %>% filter(year > 2000)
ual_us$V1 <- (ual_us$V1 - x$V1)/1000000000000



### Create a theme() for ggplot ###

plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0, 0,0,0, "cm"),
                               axis.text.y = element_text(size=8, color = "black"),
                               axis.text.x = element_text(size=8, color = "black", angle = 90, hjust = 1, vjust = 0.5),
                               axis.title.y = element_text(size=9, color = "black"),
                               axis.title.x = element_text(size=9, color = "black"),
                               legend.title = element_text(size = 8, colour = "white", face = "bold"))



reason.data$funded_ratio <- as.numeric(reason.data$funded_ratio)
funded <- reason.data[, median(na.omit(funded_ratio)), by=list(year)]
funded <- funded %>% filter(year > 2000)
funded2 <- funded
funded <- funded$V1



names(APERS.data)


###### Shiny App/Dashboard  



ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(
                      
                      titleWidth= 650,
                      title = tagList(
                      img(src='https://www.atlasnetwork.org/assets/uploads/global-directory/Screen_Shot_2019-08-19_at_11.52.56_AM.png',
                          height = 48,
                          width = 110,
                          align = "left"),
                      span(class = "logo-lg", HTML("<b>Arkansas PERS Dashboard</b>")))),
                    
                    dashboardSidebar(

                      
                      sidebarMenu(
                        
                        menuItem("APERS Dashboard", tabName = "main", icon = icon("chart-line")),
                        
                        menuItem(sliderInput('year',
                                             'Select Starting Year',
                                             min = 2001,
                                             max = max(APERS.data$year),
                                             value = 2001,
                                             step = 1,
                                             sep = ""))
                      )
                    ),
                    
                    dashboardBody(
                      ###Remove error messages
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"),
                      
                      tags$head(tags$style(HTML('
                           
                           
                           
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #FF6633;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #FF6633;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #FF6633;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #FFFFFF;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #FF6633;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #FFFFFF;
                              color: #000000;
                              }

        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #FF6633;
                              }
                              '))
                                ),
                                
                                
                      
                      
                      
                      tabItems(
                        

                        tabItem(tabName = "main",
                                
                                #tags$head(tags$style(HTML(".small-box {height: 100px}"))),#Height of valueBox
                                
                                
                                tags$style(".small-box { background-color: #FFFFFF !important; color: #333333 !important; }"),
                                
                                fluidRow(
                                
                                column(width = 12, valueBoxOutput("top_ranking2"), valueBoxOutput("top_ranking")),
                                
                                
                                column(width = 12,
                                
                                
                                tabBox(
                                  
                                  side = "left",
                                  tabPanel(title = "APERS Assets vs. Liabilities", width = 6, plotly::plotlyOutput("plot_Filtered")), 
                                  tabPanel(title = "APERS Unfunded Liability", width = 5, plotly::plotlyOutput("plot_Filtered_UAL"))),

                                box(title = "APERS vs. U.S. Funded Status", width = 6, plotly::plotlyOutput("plot_Funded"))),
                                
                                column(width = 12,
                                
                                box(title = "APERS Investment Returns", width = 6,  plotly::plotlyOutput("plot_Filtered_Returns")), 
                                box(title = "Causes of APERS Pension Debt", width = 6,  plotly::plotlyOutput("plot_waterfall")))
                                
                                )
                        )
                        
                      )
                    )
                  
)

######Shiny app[server]
##########################

server <- function(input, output, session){
  
  
  #https://reasonfoundation.github.io/R-sandbox/
  
  PlanData <- reactive({
    
    UAL <- APERS.data %>%
      filter(year >= input$year)
    
    UAL <- data.table(UAL)
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
      
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Liability, 5), color="Unfunded Actuarial Liability"), fill = "gray88", size = 0.75)+
      geom_area(data=UAL, aes(x=Fiscal_Year, y=round(Actuarial_Assets, 5)), fill = "white", size = 0.75)+
      
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Actuarial_Liability, 
                              color="Actuarial Accrued Liability", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>AAL: $",round(Actuarial_Liability, 5), "B")),
                size = 1.00)+
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Actuarial_Assets, 
                              color="Actuarial Value of Assets", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>AVA: $",round(Actuarial_Assets, 5), "B")),
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
                            " $",round(last((UAL$UAL_AVA)), 5), "B",
                            " Pension Debt", sep="\n"))
    
    p <- ggplotly(p, tooltip = c("text"))
    p <- p %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    p
  })
  
  output$top_ranking <- renderValueBox({
    
    UAL <- data.table(PlanData())
    
    UALa <- data.table("Funded_Ratio"= (UAL$funded_ratio), 
                       "Fiscal_Year"= (UAL$year)
    )
    
    
    UALb <- data.table("Funded_Ratio_US" = funded2$V1,
                       "Fiscal_Year" = funded2$year
    )
    
    
    UALa$Fiscal_Year <-as.numeric(UALa$Fiscal_Year)
    UALb$Fiscal_Year <-as.numeric(UALb$Fiscal_Year)
    
    
    UALa <- data.frame(UALa)
    UALb <- data.frame(UALb)
    
    UALa <- UALa %>%
      filter(Fiscal_Year >= input$year)
    
    UALb <- UALb %>%
      filter(Fiscal_Year >= input$year)
    
    
    UAL <- left_join(UALa, UALb)
    

    valueBox(value = tags$p(ifelse(median(na.omit(UAL$Funded_Ratio))>median(na.omit(UAL$Funded_Ratio_US)), "Funded Above US Median*",  "Funded Below US Median*"), style = "font-size: 80%;"), HTML(paste0(
      "Plan Median Funded: ", round(median(na.omit(UAL$Funded_Ratio))*100, 2), "%","<br>",
      "US Median Funded: ", 
      round(median(na.omit(UAL$Funded_Ratio_US))*100, 2), "%",sep="<br>")), 
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
      "$", round(last(na.omit(UAL$UAL_AVA)), 5), 
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
                                                       Data[year == min(year)]$actuarial_value_of_assets_gasb_dollar))/1000000,5)
                           ),"M"),
        end_ual = paste0(last(as.numeric(year)), ": ", 
                         (round(na.omit(as.numeric(Data[year == last(year)]$actuarially_accrued_liabilities_dollar-
                                                     Data[year == last(year)]$actuarial_value_of_assets_gasb_dollar))/1000000,5)
                         ),"M")
      )
  })
  
  
  output$plot_waterfall <- plotly::renderPlotly({
    
    df <- PlanData()
    
    df$investments <- as.numeric(df$investments) * 1e3
    df$benefit_changes <- as.numeric(df$benefit_changes) * 1e3
    df$assumption_method_changes <- as.numeric(df$assumption_method_changes) * 1e3
    df$negative_amo <- as.numeric(df$negative_amo) * 1e3
    df$demographic <- as.numeric(df$demographic) * 1e3
    df$pay_increase <- as.numeric(df$pay_increase) * 1e3
    
    
    x= list("Underperforming Investments",
            "Benefit Changes & Other",
            "Changes in Methods and Assumption",
            "Negative Amortization",
            "Actual Demographic Performance",
            "Pay Increase not given",
            "Net Change to Unfunded Liability")
    
    measure= c("relative",
               "relative",
               "relative",
               "relative",
               "relative",
               "relative",
               "total")
    
    y <- c(sum(df$investments),
           sum(df$benefit_changes),
           sum(df$assumption_method_changes),
           sum(df$negative_amo),
           sum(df$demographic),
           sum(df$pay_increase), 0)
    
    data <- data.frame(x = factor(x, levels = x), measure, y)
    
    
    
    fig <- plot_ly( data,
                    type = "waterfall",
                    measure = ~measure,
                    x = ~x,
                    textposition = "outside",
                    y= ~y,
                    text = "",
                    hoverinfo = 'text',
                    hovertemplate = paste('%{y:$,.0f}<extra></extra>'),
                    decreasing = list(marker = list(color = "#669900")),
                    increasing = list(marker = list(color = "#CC0000")),
                    totals = list(marker = list(color = "#FFCC33")),
                    
                    connector = list(line = list(color= "#333333", width = 1))) 
    
    fig <- fig %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "Change in Unfunded Liability (Billions)"),
             autosize = T,
             showlegend = F)
    
    fig
    
  })
  
  
  output$plot_Funded <- plotly::renderPlotly({
    UAL <- data.table(PlanData())
    
    UALa <- data.table("Funded_Ratio"= (UAL$funded_ratio), 
                      "Fiscal_Year"= (UAL$year)
                      )
    

    UALb <- data.table("Funded_Ratio_US" = funded2$V1,
                       "Fiscal_Year" = funded2$year
                       )
    

    UALa$Fiscal_Year <-as.numeric(UALa$Fiscal_Year)
    UALb$Fiscal_Year <-as.numeric(UALb$Fiscal_Year)
    
    
    UALa <- data.frame(UALa)
    UALb <- data.frame(UALb)
    
    UALa <- UALa %>%
      filter(Fiscal_Year >= input$year)
    
    UALb <- UALb %>%
      filter(Fiscal_Year >= input$year)
    
    
    UAL <- left_join(UALa, UALb)
    
    
    u <- ggplot() +
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Funded_Ratio, 
                              color="Funded Status", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            " <br>Funded Status: ",round(Funded_Ratio,2)*100, "%")),
                size = 1)+
      
      geom_line(data=UAL, aes(x=Fiscal_Year, y=Funded_Ratio_US, 
                              color="Funded Status US", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            " <br>Funded US Status: ",round(Funded_Ratio_US,2)*100, "%")),
                size = 1)+
      
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
                                            "<br>UAL: $",round(UAL_AVA, 5), "B")),
                fill = "orangered2"#, alpha = 1, size = 1
      ) +
      
      scale_colour_manual(values=c("orangered2")) +
      
      scale_y_continuous(labels = function(x) paste0("$",x,"B"), name = "") +
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(min(UAL$Fiscal_Year), 2019, by = 1), limits = c(min(UAL$Fiscal_Year), 2019))+
      theme_bw() +
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
    
    UAL4$Fiscal_Year <- as.numeric(UAL4$Fiscal_Year)
    
    UAL4 <- data.frame(UAL4)
    
    k <- ggplot() +
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

    UAL5 <- data.table("ADEC"= as.numeric(UAL5$adec), 
                       "ADEC_Paid"= (as.numeric(UAL5$adec)*as.numeric(UAL5$adec_paid_pct)), 
                       "Fiscal_Year"= as.numeric(UAL5$year)
    )
    
    UAL5 <- na.omit(UAL5)
    UAL5$ADEC <- as.numeric(UAL5$ADEC)
    UAL5$ADEC_Paid <- as.numeric(UAL5$ADEC_Paid)
    #UAL5 <- na.omit(UAL5)
    
    UAL5 <- data.frame(UAL5)

    
    c <- ggplot() +
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ADEC,
                              color="ADEC Not Paid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC Not Paid: $",round(ADEC/1000000, 2), " Million")), width = 0.7, 
               fill = "orangered1"
      )+
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ifelse((ADEC_Paid>ADEC), ADEC_Paid,0),
                              color="ADEC OverPaid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC OverPaid: $",round((ADEC_Paid-ADEC)/1000000, 2), 
                                            " Million")), width = 0.7, 
               fill = "green3"
      )+
      
      geom_col(data=UAL5, aes(x=Fiscal_Year, y=ifelse((ADEC_Paid<=ADEC), ADEC_Paid/1000000,ADEC),
                              color="ADEC Paid", group =1,
                              text = paste0("Fiscal Year: ", Fiscal_Year,
                                            "<br>ADEC Paid: $",ifelse((ADEC_Paid<=ADEC), round(ADEC_Paid/1000000, 2),round(ADEC/1000000, 2)), 
                                            " Million")), width = 0.7, 
               fill = "grey80"
      )+
  
      geom_line(data=UAL5, aes(x=Fiscal_Year, y=ADEC, 
                               color="ADEC", group =1,
                               text = paste0("Fiscal Year: ", Fiscal_Year,
                                             "<br>ADEC: $",round(ADEC/1000000, 2), " Million")), fill = "gold2", size=0.8
      )+
      
      scale_colour_manual(values=c(palette_reason$Yellow, "white", "white", "white"))+
      scale_y_continuous(labels = function(x) paste0("$", round((x/1000000), 5), "M"), name = "Employer Contributions vs. ADEC",
      )+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                         breaks = seq(input$year-1, (last(UAL5$Fiscal_Year)+1), by = 1), limits = c(input$year-1, (max(UAL5$Fiscal_Year)+1)))+
      theme_bw()+

      plotTheme
    
    c <- ggplotly(c, tooltip = c("text"))
    c <- c %>% layout(autosize = TRUE, legend = list(orientation = "v", x=0.01, y = 1))
    c
  })
  
}


shinyApp(ui = ui, server = server)









