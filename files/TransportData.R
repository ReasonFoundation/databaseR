### Transportation database app ###

### Packages
rm(list=ls())

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

###
#### SQL query ####

### Connection 
con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = "d629vjn37pbl3l",
  host = "ec2-3-209-200-73.compute-1.amazonaws.com",
  port = 5432,
  user = "reason_readonly",
  password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
  sslmode = "require")

### Query
query <- "select * from transportation.ahr"
result <- RPostgres::dbSendQuery(con, query)
all_data <- RPostgres::dbFetch(result) %>% janitor::clean_names()
RPostgres::dbClearResult(result)
RPostgres::dbDisconnect(con)
#####################

transport.data <- all_data

### all column names
variables <- colnames(all_data)

### Filter specifica columns
#transport.data <- transport.data %>% select(state, year, rank_overall_performance, rank_maint_as_a_pct_of_bdgt)
#on
###
#View(transport.data)

#Create shiny app w/:
#1. Select/Filter by state [*]
#2. Filter by year [*]
#3. Select columns
#4. Download excel/csv [*]
#5. Visualize (Rankings, etc)
#6. Merge w/ pension data (inflation, etc) by state 
#-> admin exp. adjusted for inflation*
#-> admin exp. adjusted for population*
#7. post [*]



############
######Shiny app[ui] #######

ui <- fluidPage(
  br(),
  img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("Transportation Data Viewer V1.0"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  
  mainPanel(
    ###Remove error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabsetPanel(
      tabPanel("2001-21 Data", 
               sliderInput('year', 'Select Starting Year', min = 2001, max = 2021, value = 2005, sep = ""),
               downloadButton("downloadData", "Download Data"),#, width = 3,
               pickerInput("pk.state", "Select States", 
                           choices = c(as.character(unique(transport.data$state))),
                           selected = c(as.character(unique(transport.data$state))),
                           multiple = T,
                           options = list(`actions-box` = TRUE)),
               radioGroupButtons("filter", "Select Columns", choices = c("Yes", "No"), selected = "No",
                                 status = "primary"),
               uiOutput("second"),
               DT::DTOutput("plot_TranspData")),
      
      #https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288),
      tabPanel("Aggregate Ranking", 
               sliderInput('year2', 'Select Year', min = 2001, max = 2021, value = 2021, sep = ""),
               switchInput(
                 inputId = "perc",
                 label = "Percentiles", 
                 labelWidth = "70px",
               ),
               plotly::plotlyOutput("plot_Return_Distribution")),
      
      tags$div(htmlOutput("text1")),
      
      
      id = "tabset"
    ), 
    id="main"
    
  )
)


##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  output$plot_TranspData <- DT::renderDT({
    
    
    transport.data <- DT::datatable( transport.data 
                                     %>% filter(year >= input$year, state %in% input$pk.state) 
                                     %>% select(state, year, report_edition,  if(input$filter == "Yes"){input$pk}else{c(variables)})
                                     %>% arrange(year, state))
    transport.data   })
  
  
  
  datasetInput <- reactive({
    x <-  (transport.data 
    %>% filter(year >= input$year, state %in% input$pk.state) 
    %>% select(state, year, report_edition,  if(input$filter == "Yes"){input$pk}else{c(variables)}) 
    %>% arrange(year, state))
    x
  })
  
  #### show column picke rupon pressing "Yes"
  output$second  <- renderUI({
    
    if(input$filter == "Yes"){
      
      pickerInput("pk", "Select Columns", 
                  choices = c(variables),
                  selected = c(variables),
                  multiple = T,
                  options = list(`actions-box` = TRUE))
      
    } else {
      NULL
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Transportation Data Download_",input$year, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}
####
shinyApp(ui = ui, server = server)
###