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
library(extrafont)
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
library(bootstrap)
#library(shinyFiles)
library(DT)
library(plotly)

#palette_reason <- data.table(
#  Orange = "#FF6633", 
#  LightOrange = "#FF9900",
#  DarkGrey = "#333333", 
#  LightGrey = "#CCCCCC",
#  SpaceGrey = "#A69FA1",
#  DarkBlue = "#0066CC",
#  GreyBlue = "#6699CC", 
#  Yellow = "#FFCC33", 
#  LightBlue = "#66B2FF", 
#  SatBlue = "#3366CC", 
#  Green = "#669900",
#  LightGreen = "#00CC66",
#  Red = "#CC0000",
#  LightRed = "#FF0000")
#palette

##x <- pullStateData(2001)
#x <- data.matrix(x)
#x <- data.table(x)
#class(x$investment_return_assumption_for_gasb_reporting)  
#x <- sapply(x, as.numeric)

#x <- x  %>%
#  mutate_all(type.convert) %>%
#  mutate_if(is.factor, as.character)

#library(extrafont)
#font_import()
#fonts()
#loadfonts(device="win")

#### Load/Filter #####
pl <- planList() 
#View(arrange(pl, by = id))
states <- as.character(unique(pl[,3]))
plans <- as.character(unique(pl[,2]))

#font_import(pattern = "Arial")
#View(colnames(pullStateData(2001)))
#Ideas --
#https://rud.is/b/2016/04/17/ggplot2-exercising-with-ggalt-dumbbells/

#loadfonts(device = "win")
#dir.create('~/.fonts')
#file.copy("/Users/anilniraula/Arial .ttf", "~/.fonts")
##
# Load data ---------------------------------------------------------------

reason.data <- pullStateData(2001) 
reason.data <- filterData(reason.data, 2001)
reason.data <- data.table(reason.data)
reason.data <- data.table(reason.data) %>% arrange(state,plan_name,year)
reason.data <- reason.data %>% filter(plan_name != "Colorado PERA, School Division Fund")
reason.data <- reason.data %>% filter(plan_name != "Oregon Public Service Retirement Plan")
#Manually imputed data for NC (2001), Arkasnas/Colorado/Alabama (2019)
reason.data[state == "North Carolina" & year == 2001]$aal <- c(9967547769, 35248769986)
reason.data[state == "Arkansas" & year == 2019]$mva <- c(8803211537, 17741621773)
reason.data[state == "Arkansas" & year == 2019]$aal <- c(11129000000, 21708867019)
reason.data[state == "Colorado" & year == 2019]$mva <- c(4545960000, 15819843000)
reason.data[state == "Colorado" & year == 2019]$aal <- c(5316433000, 25717648000)
reason.data[state == "Alabama" & year == 2019]$mva <- c(12568473000, 25619448000)
reason.data[state == "Alabama" & year == 2019]$aal <- c(18543542000, 37215470000)
reason.data[state == "Hawaii" & year == 2019]$mva <- reason.data[state == "Hawaii" & year == 2019]$mva_smooth
reason.data[state == "Montana" & year == 2001]$aal <- c(0, 0)
reason.data[state == "Nebraska" & year == 2001]$aal <- c(0)
reason.data[state == "Wisconsin" & year == 2019]$mva <- reason.data[state == "Wisconsin" & year == 2018]$mva
reason.data[state == "Wisconsin" & year == 2019]$aal <- reason.data[state == "Wisconsin" & year == 2018]$aal
reason.data[state == "Wyoming" & year == 2019]$mva <- reason.data[state == "Wyoming" & year == 2018]$mva
reason.data[state == "Wyoming" & year == 2019]$aal <- reason.data[state == "Wyoming" & year == 2018]$aal
#reason.data[state == "Oregon" & year == 2018]$mva <- c(69327500445)

#View(reason.data[state == "Wisconsin"])
#reason.data[state == "Ohio" & year == 2019]$mva <- NA

#View(reason.data[state == "North Carolina" & year == 2001])
#View(reason.data[state == "Colorado" & year > 2017])

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod2.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))
#View(plan.names)

for (i in 1:plan.names[,.N]){
  reason.data[plan_name %in% plan.names[i,1]]$plan_name <- as.character(plan.names[i,2])
}

#View(reason.data[state == "Colorado"])

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
reason.data <- data.table(na.omit(reason.data, cols=c("mva", "aal")))
reason.data[state == "Oregon" & year == 2018]$mva <- c(69327500445)
reason.data2 <- data.table(reason.data[, lapply(.SD, sum, na.rm=TRUE), by=list(year, state), .SDcols=c("mva", "aal")])
reason.data2[,funded := mva/aal]
#View(reason.data2)
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

#View(reason.data[state == "Arkansas" & year == 2019])
#https://rkabacoff.github.io/datavis/datavis.pdf
#set_reason_theme(style = "slide")

set_reason_theme(style = "web")

###SHINY DASHBOARD APP
ui <- fluidPage(
      h1(id = "title", "State Public Pension Plans: Funded Status Changes Over Time"),
      #Set Title font
      tags$head(tags$style(
         HTML('#title {
                font-size: 20pt; 
                color: black;
                font-style: Arial;
             }'))),
      theme = shinythemes::shinytheme("spacelab"), 
      #Set Control Panel text font
      tags$style(HTML("body, pre { 
                font-size: 10pt; 
                color: black;
                font-style: Arial}")),
      
  #Change font of the body text
  #https://stackoverflow.com/questions/58454087/how-to-change-the-font-family-of-verbatimtextoutput-to-be-the-same-as-the-input
  sidebarLayout(
    sidebarPanel(width = 4, img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 55),
      sliderInput('x', h3('Select Time Period'), 
                           min = 2001, max = 2019, value = c(2001,2019), sep = ""),
     uiOutput("secondSelection"),
     # em("Choose Starting and Ending Years to customize any period between 2001 and 2018"),
     uiOutput("thirdSelection"),
     selectInput("s", "Funded Status Change by State", choices = states),
     htmlOutput("top_ranking"),
      ),
      ###Remove error messages
     mainPanel(
       ###Remove error messages
       tags$style(type="text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"
       ),
       plotly::plotlyOutput("plot_US"),
          tags$div(HTML(paste("<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>"))),
          
       tags$div(htmlOutput("text1"))
       
          #Specify Source line text font/size with css
          ))
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
          "analysis of U.S. public pension actuarial valuation reports and CAFRs.", "<br>", "<br>", 
          "<b>Methodology and Notes</b>:", "Funded Status numbers shown represent aggregate Market Value of Assets as a share of aggregate Actuarial Accrued Liability.", "<br>", 
          "Montana & Nebraska data for 2001 is incomplete.", "<br>", "Wisconsin & Wyoming data for 2019 is not yet available and shows 2018 values.","<br>", 
          "Some plans are yet to disclose 2019 data. ", sep="\n")
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
    reason.data <- data.table(na.omit(reason.data, cols=c("mva", "aal")))
    reason.data[state == "Oregon" & year == 2018]$mva <- c(69327500445)
    reason.data2 <- data.table(reason.data[, lapply(.SD, sum, na.rm=TRUE), by=list(year, state), .SDcols=c("mva", "aal")])
    reason.data2[,funded := mva/aal]
    #View(reason.data2)
    reason.data2 <- data.table(reason.data2)
    reason.data2$funded <- as.numeric(reason.data2$funded)
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
             color = "white", size = 2) +
    scale_fill_manual(values=c(palette_reason$LightBlue, palette_reason$Orange, palette_reason$Yellow, palette_reason$LightBlue, palette_reason$Red, palette_reason$DarkGrey))+
    geom_point(aes(x = DF[,3], xend = DF[,3], y = reorder(state, gap), group = state
                   ),col = palette_reason$Orange, size = 2) + 
    geom_point(aes(x = DF[,2], xend = DF[,2], y = reorder(state, gap), group = state, fill = c("Starting Funded Status")),
               col = palette_reason$LightBlue, size = 2) + 
    geom_col(aes(x = ifelse((DF[,3]>DF[,2]), DF[,2], DF[,3])-0.005, xend = DF[,3]-0.005, y = reorder(state, gap), group = state,
                 text = paste0("")),
              fill = "white", size = 2) +
    #geom_vline(xintercept=(median(DF[,3]+median(DF[,4]))), text = paste0("Median US Funded Status"),
    #           linetype="dashed", 
    #           color = palette_reason$DarkGrey, size=1)+
    
  
    labs(title= paste0("State Pension Funded Gap (",input$x[1], "-", input$x[2],")"),
         x = "Funded Ratio", y = ""
         )+
   # theme_set(theme_bw( base_family= "Arial"))+
    annotate("text", fontface = 'bold', size = 3.3, x=1.5, y=2,
             label = paste0("reason.org/pensions"))+
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
                      axis.title = element_text(family = "Arial Unicode MS", face = "bold", color = "black"),
                      axis.text = element_text(family = "Arial Unicode MS", face = "bold", color = "black"),
                      axis.line.x = element_line(size = 1, colour = 'black'),
                      axis.line.y = element_line(size = 1, colour = 'black'),
                      axis.ticks.x = element_line(size = 1, color="black"), 
                      axis.ticks.y = element_line(size = 1, color="black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.title = element_text(size = 9, colour = "white", face = "bold"),
                      plot.title=element_text(family="Arial Unicode MS", face="bold", size=15))+#Set Plot Title font
    expand_limits(x = 0, y = 0)
  
    p <- ggplotly(p, tooltip = c("text"))
    p <- p %>% layout(legend = list(orientation = "v", x=0.65, y = 0.5, font = list(size = 12, family = "Arial")),#Set legend fonts 
                      xaxis = list(tickfont = list(size = 13.5, family = "Arial Unicode MS")), #Set axi,s label fonts 
                      yaxis = list(tickfont = list(size = 13.5, family = "Arial Unicode MS")),
                      hoverlabel = list(font = list(family = "Arial Unicode MS", 
                                                    size = 12, 
                                                    color = "black"))) 
    p = p %>%
      #https://mran.revolutionanalytics.com/snapshot/2016-03-14/web/packages/plotly/plotly.pdf
      config(staticPlot = F, 
             editable = F,
             autosizable = T,
             showTips = T,
             displayModeBar = T,#Removing the all the buttons
             modeBarButtonsToRemove = list(
               #"toImage",
               #"zoom2d",
               "pan2d",
               "lasso2d",
               "lasszoom2d",
               "pan2d",
               "select2d",
               "zoomIn2d",
               "zoomOut2d",
               #"autoScale2d",
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


output$top_ranking <- renderText({

  reason.data <- reason.data[!(state == "Wyoming" & year == 2019), ]
  reason.data <- reason.data[!(state == "Wisconsin" & year == 2019), ]
  #View(reason.data)
  x <- sum(reason.data[year==input$x[1]]$mva)/sum(reason.data[year==input$x[1]]$aal)
  #View(x)
  y <- sum(reason.data[year==input$x[2]]$mva)/sum(reason.data[year==input$x[2]]$aal)
  #View(y)
  US.funded <- y-x
  #View(US.funded)
  
  DF <- data.frame(PlanData())
  DF <- DF %>% filter(state == input$s)
  State.funded <- -DF[,4]
  #View(State.funded)
  
           HTML(paste0(  
                "<B>", input$s, " (",input$x[1],"-", input$x[2],"): ", "</B>","<br>","Funded Status", 
                ifelse(na.omit(State.funded)<0,paste0(" Declined "),
                paste0(" Increased ")), "<B>",
                ifelse(is.na(US.funded), paste("data not available"), round(na.omit(State.funded)*100,1)), " perc. points","</B>","<br>",
                "<B>","All US Plans (",input$x[1],"-", input$x[2],"): ","</B>", "<br>"," Funded Status", 
                ifelse(na.omit(US.funded)<0,paste0(" Declined "),
                paste0(" Increased ")),"<B>",round(na.omit(US.funded)*100,1), " perc. points","</B>"))

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
    labs(caption = paste("reason.org/pensions"), family = "Arial Unicode MS")+
    theme_bw()+
    ggplot2::theme(
                      panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                      plot.margin = margin(0, 0,0,0, "cm"),
                      #plot.margin = margin(0.1, 3,0,3, "cm"),
                      axis.text.y = element_text(size=8, color = "black"),
                      axis.text.x = element_text(size=8, color = "black", angle = 90, hjust = 1, vjust = 1),
                      axis.title.y = element_text(size=9, color = "black"),
                      axis.title.x = element_text(size=9, color = "black"),
                      legend.title = element_text(size = 5, colour = "white", face = "bold"))#+
   # coord_cartesian(expand = FALSE, #turn off axis expansion (padding)
    #                xlim = c(input$x[1],input$x[2]), ylim = 0, max(UALFunded_Status)*1.1) #manually set limits
  
  
  p <- ggplotly(p, tooltip = c("text"))
  p = p %>%
    #https://mran.revolutionanalytics.com/snapshot/2016-03-14/web/packages/plotly/plotly.pdf
    config(staticPlot = F, 
           editable = F,
           autosizable = T,
           showTips = T,
           displayModeBar = F,#Removing the all the buttons
           modeBarButtonsToRemove = list(
             #"toImage",
             #"zoom2d",
             #"pan2d",
             #"lasso2d",
             #"lasszoom2d",
             #"pan2d",
             #"select2d",
             #"zoomIn2d",
             #"zoomOut2d",
             #"autoScale2d",
             #"resetScale2d",
             #"hoverClosestCartesian",
             #"hoverCompareCartesian",
             #"sendDataToCloud",
             #"toggleHover",
             #"resetViews",
             #"toggleSpikelines",
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
#########