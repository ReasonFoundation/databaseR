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

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020.csv"
returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
returns_2020$mva_billions <- as.numeric(returns_2020$mva_billions) 
returns_2020$arr <- as.numeric(returns_2020$arr) 
#View(returns_2020$arr)

pl <- planList()


ui <- fluidPage(
  img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("State Pension 2019-20 Returns"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(width = 5, 
      radioGroupButtons("filter", "Filter by", choices = c("Highest returns", "Return Assumption", "Asset size"))
    ),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel('Table', DT::DTOutput('plot_Returns'))    
      )
    )
  )
)
##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  output$plot_Returns <- DT::renderDataTable({
   
    returns_2020 <- as.data.table(returns_2020)
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    returns_2020<- returns_2020 %>% arrange(plan_name)
    returns_2020$mva_billions <- round(returns_2020$mva_billions,1)
    colnames(returns_2020) <- c("Pension Plan", "State", "2020FY Return", "Return Assumption", "Market Assets (Billions)", "Source")
    
    returns_2020 <- (
          if(input$filter == "Highest returns"){
           returns_2020 %>% arrange(desc(returns_2020[,3]))
        } else if(input$filter == "Asset size"){
          returns_2020 %>% arrange(desc(returns_2020[,5]))
        } else{returns_2020 %>% arrange(desc(returns_2020[,4]))})
    
    returns_2020 <- datatable(returns_2020) %>% formatPercentage(c("2020FY Return", "Return Assumption"), 1)
  
    #http://www.stencilled.me/post/2019-04-18-editable/
    #returns_2020 <- DT::datatable(returns_2020, editable = FALSE)#set to TRUE to allow editing
    
    #returns_2020 <- as.data.table(returns_2020)
    #returns_2020[,one_year_gain_loss := (returns_2020$mva_billions * (returns_2020$arr - returns_2020$`2020_return`))]
  })
  
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
