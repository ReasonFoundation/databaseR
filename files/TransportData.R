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
all.columns <- colnames(all_data)

### Filter specifica columns
transport.data <- transport.data %>% select(state, year, rank_overall_performance, rank_maint_as_a_pct_of_bdgt)

###
#View(transport.data)

#Create shiny app w/:
#1. Filter by state
#2. Filter by year
#3. Select columns
#4. Download excel/csv



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
      tabPanel("2001-20 Return Distribution", 
               sliderInput('year', 'Select Starting Year', min = 2001, max = 2021, value = 2001, sep = ""),
               prettyRadioButtons("lines", "Add Lines", 
                                  choices = c("None", "Median Assumed Rate of Return (ARR)","30Y Treasury Yield"), selected = "None", status = "warning"),
               DT::DTOutput("plot_InvReturns")),
      
      #https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288),
      tabPanel("Year-By-Year Return Distribution", 
               sliderInput('year2', 'Select Year', min = 2001, max = 2020, value = 2020, sep = ""),
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
 
  output$plot_InvReturns <- DT::renderDT({
    
    
    transport.data <- DT::datatable( transport.data)
    transport.data
  })
   
}
####
shinyApp(ui = ui, server = server)
