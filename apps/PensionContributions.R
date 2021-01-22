
### 2001-2020 Investment Returns ###
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
library(grid)#https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
library(tidyverse)
#library(openxlsx)
library(tseries)
library(plyr)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
library(shinyjs)
#library(shinyFiles)
library(DT)
library(plotly)
library(httr)
library(jsonlite)


#load("~/pensionviewr/data/palette_reason.rda")

#https://publicplansdata.org/public-plans-database/api/#examples
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/API-data-access-r/
#url_ppd <- 'http://publicplansdata.org/api/'
#repos <- GET(url = paste0(url_ppd, "q=ListVariables", "json"))
#repos
#content(repos, q=ListVariables, format=xml)
#names(repos)  
#Headers
#repos$all_headers
#Date of the content API
#repos$date
#

#install.packages(c("httr", "jsonlite"))
#devtools::install_github("ropensci/plotly")
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data,2001)
reason.data <- reason.data %>% select(year, state, plan_name
                                      #,adec
                                      #,adec_paid_pct
                                      ,statutory_pct
                                      #,er_nc_pct
                                      ,er_proj_adec_pct
                                      ,ee_nc_pct
                                      ,total_amortization_payment_pct
                                      ,total_nc_pct)
#,total_proj_adec_pct)
reason.data <- reason.data %>% 
arrange(desc(er_proj_adec_pct, year, state))

#View(reason.data)
#write.csv(reason.data, "reason.data.csv", row.names = FALSE)


############
######Shiny app[ui] -------------------------------------------------

ui <- fluidPage(
  br(),
  img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("State Pension Contributions"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  
  mainPanel(
    ###Remove error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabsetPanel(
      tabPanel("Employer/Employee Contributions (%)", 
               sliderInput('year', 'Select Starting Year', min = 2001, max = 2020, value = 2001, sep = ""),
               DT::DTOutput('plot_Contributions'), style = "font-size:100%; width:50%")
      
      #https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288),
      
      #tags$div(htmlOutput("text1")),
    )
  )
)


##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  observeEvent(input[["tabset"]], {
    
    hideElement(selector = "#sidebar")
    removeCssClass("main", "col-sm-8")
    addCssClass("main", "col-sm-12")
  })
  
  
  output$plot_Contributions <- DT::renderDataTable({
    
                                          #View(reason.data)
                                          
                                          
    reason.data <- data.table(reason.data)
    reason.data$year <- as.numeric(reason.data$year)
    reason.data <-  reason.data %>% filter(year >= input$year)
    #Calculating Approximate Investment Loss
    colnames(reason.data) <- c("Year", "State", "Plan", "ER Statutory Rate", "ER ADEC Rate", "EE NC Rate", "Total Amo. Rate", "Total NC Rate")
    #reason.data[,4:8] <-  reason.data[,4:8] %>% dplyr::mutate_all(dplyr::funs(as.numeric))
    

    
    reason.data <- as.data.frame(reason.data) 
    #View(reason.data)
    contr.rates <- DT::datatable(reason.data, escape = FALSE, filter = "top", editable = FALSE, 
                                  options = list(pageLength = 40, autoWidth = TRUE, dom = 't'), rownames= FALSE) %>% 
      #escape = FALSE keeps the a href links
      #filter = "top" add quick search bars below column titles
      #dom = 't' removes list and search bars at the top
      
      #formatStyle(#"FY19-20 Returns", 
                  #background = styleColorBar(returns_2020[,3], palette_reason$LightBlue),
                  #backgroundSize = '98% 88%',
                  #backgroundRepeat = 'no-repeat',
                  #backgroundPosition = 'right',
                  #fontWeight = styleEqual(10, "bold"))# %>%
      
      formatPercentage(c("ER Statutory Rate", "EE NC Rate", "ER ADEC Rate", "Total Amo. Rate", "Total NC Rate"),1) #%>%
      
      #formatCurrency(c("Market Assets (Billions)", "Estimated Investment Loss (Billions)"))
  
  })
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
