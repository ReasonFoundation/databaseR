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
library(httr)
library(jsonlite)

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

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020.csv"
returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
returns_2020$mva_billions <- as.numeric(returns_2020$mva_billions) 
returns_2020$mva_billions_19 <- as.numeric(returns_2020$mva_billions_19) 
returns_2020$arr <- as.numeric(returns_2020$arr) 
returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
returns_2020<- returns_2020 %>% arrange(plan_name)


#https://community.plotly.com/t/r-plotly-overlay-density-histogram/640/3
#returns_2020<- data.matrix(na.omit(returns_2020))
#fit <- density(returns_2020[,3])

returns_2020<- data.frame(returns_2020)

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
returns_2020$mva_billions_19 <- as.numeric(returns_2020$mva_billions_19) 
returns_2020$arr <- as.numeric(returns_2020$arr) 
returns_2020$`2020_return` <- as.numeric(returns_2020$`2020_return`)
#View(returns_2020)

pl <- planList()
d1 <- density(na.omit(returns_2020[,3]))
plot((d1$y)/sum(d1$y))
fig <- plot_ly(x = ~d1$x, y = (d1$y)/sum(d1$y), type = 'scatter', mode = 'lines', name = 'Fair cut', fill = 'tozeroy')
fig
#https://chart-studio.plotly.com/~vigneshbabu/9/_10th-percentile-25th-percentile-median-75th-percentile-90th-percentile/#/


ui <- fluidPage(
  img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("State Pension 2019-20 Returns"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(width = 5, 
      radioGroupButtons("filter", "Sort by", choices = c("Plan Name", "Highest returns", "Assumed Rate of Return", "Asset size"), selected = c("Plan Name")),
    ),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel('Investment Returns by Plan', DT::DTOutput('plot_Returns'),
                      style = "font-size:80%; width:50%"),
                  #https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288),
        tabPanel('Return Distribution', plotly::plotlyOutput("plot_Return_Distribution")),
  
        tags$div(htmlOutput("text1")))#Adjusting font size & width
      )
    )
  )
##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  output$text1 <- renderText({
    paste(HTML(
      "Source:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"),"<br>", 
      "analysis of state pension plan repoted returns, CAFRs and valuation reports.","<br>", "<br>", 
      "Methodology: 'Approximate Recognized Investment Loss' is calculated by","<br>", 
      "taking plan's FY2018-19 'Market Value of Assets' and multiplying it by the difference between 'Assumed Rate of Return' and 'FY2019-20 Return'. 
      Values are meant as an approximation of recognized losses due to in FY19-20 returns deviating from assumed return","<br>", "<br>",
      "**Preliminary returns",sep="\n")
  })
  
  
  output$plot_Returns <- DT::renderDataTable({
   
    returns_2020 <- data.table(returns_2020)
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    returns_2020<- returns_2020 %>% arrange(plan_name)
    returns_2020<- data.table(returns_2020)
    returns_2020[, inv_gain_loss := mva_billions_19*(arr-`2020_return`)]
    returns_2020$inv_gain_loss <- round(returns_2020$inv_gain_loss,3)
    returns_2020$mva_billions <- round(returns_2020$mva_billions,1)
    returns_2020$mva_billions_19 <- as.character(round(returns_2020$mva_billions_19,1))
    colnames(returns_2020) <- c("Pension Plan", "State", "FY19-20 Returns", "Assumed Rate of Return", "Market Assets (Billions)", "2019 Market Assets (Billions)", "Source", "Approximate Recognized Investment Loss (Billions)")
    returns_2020[,8] <- round(returns_2020[,8],1)
    returns_2020 <- returns_2020 %>% select("Pension Plan", "State", "FY19-20 Returns", "Assumed Rate of Return", "Market Assets (Billions)", "Approximate Recognized Investment Loss (Billions)", "Source")
    returns_2020 <- (
          if(input$filter == "Highest returns"){
           returns_2020 %>% arrange(desc(returns_2020[,3]))
        } else if(input$filter == "Asset size"){
          returns_2020 %>% arrange(desc(returns_2020[,5]))
        } else if(input$filter == "Assumed Rate of Return"){
          returns_2020 %>% arrange(desc(returns_2020[,4]))
        } else{
          returns_2020 %>% arrange(returns_2020[,1])
        }
        )
    
    
    #returns_2020 <- datatable(returns_2020) %>% formatPercentage(c("FY19-20 Returns Return", "Assumed Rate of Return"), 1)
    #http://www.stencilled.me/post/2019-04-18-editable/
    returns_2020 <- DT::datatable(returns_2020, editable = FALSE, options = list(
      "pageLength" = 40, autoWidth = TRUE)) %>% 
      formatStyle(1, fontWeight = styleEqual(10, "bold")) %>%
      formatPercentage(c("FY19-20 Returns", "Assumed Rate of Return"),2)
    #set to TRUE to allow editing
    
    
    #returns_2020 <- as.data.table(returns_2020)
    #returns_2020[,one_year_gain_loss := (returns_2020$mva_billions * (returns_2020$arr - returns_2020$`2020_return`))]
  })
  output$plot_Return_Distribution <- plotly::renderPlotly({
    
    urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020.csv"
    returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
    returns_2020$mva_billions <- as.numeric(returns_2020$mva_billions) 
    returns_2020$mva_billions_19 <- as.numeric(returns_2020$mva_billions_19) 
    returns_2020$arr <- as.numeric(returns_2020$arr) 
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    returns_2020<- returns_2020 %>% arrange(plan_name)
    
    #https://community.plotly.com/t/r-plotly-overlay-density-histogram/640/3
    #returns_2020<- data.matrix(na.omit(returns_2020))
    #fit <- density(returns_2020[,3])
    
    returns_2020<- data.frame(returns_2020)
    #write.csv(returns_2020, file = "/Users/anilniraula/Downloads/returns_2020.csv")
    #View(returns_2020[,3])
    #Creating a histogram (frequiency analysis w/ density line (smoothed histogram))
    #https://mathisonian.github.io/kde/
    #Using input "bins" to specify number of X-axis buckets
    
    #https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
    #myhist <- hist(na.omit(returns_2020[,3]))
    #multiplier <- myhist$counts / myhist$density
    #mydensity <- density(na.omit(returns_2020[,3]))
    #mydensity$y <- mydensity$y * multiplier[1]
    
    #plot(myhist)
    #lines(mydensity)
    
    #https://chart-studio.plotly.com/~vigneshbabu/9/_10th-percentile-25th-percentile-median-75th-percentile-90th-percentile/#/
    #https://plotly.com/r/mixed-subplots/
    annotation <- list(yref = 'paper', xref = "x", y = 0.5, x = median(na.omit(returns_2020[,3])), text = paste("Median = ", round(median(na.omit(returns_2020[,3]))*100,1), "%"))
    #https://plotly.com/chart-studio-help/histogram/
    d1 <- density(na.omit(returns_2020[,3]), bw = "SJ")
    
    
    quantile <- quantile(na.omit(returns_2020[,3]), c(0.25, 0.5, 0.75))
    
    histogram <- plot_ly(x = ~na.omit(returns_2020[,3]),
                  type = "histogram",
                  histnorm = "probability", 
                  nbinsx = 6, 
                  marker = list(color = if(returns_2020[,3] < quantile[1]*100){palette_reason$Orange}else{"blue"},
                                line = list(color = "black",
                                            width = 2))) %>% 
      
      add_trace(x = d1$x,
                y = 100*(d1$y/sum(d1$y)),
                type="scatter",mode = 'lines', fill = 'toze roy', showlegend = F) %>%
      

     # add_trace(x = fit$x, y = fit$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
      #
      add_segments(x = quantile[2], 
                   xend =  quantile[2], y = 0.01, yend = 0.47, showlegend = T, 
                   name = "50th Percentile", line = list(color = palette_reason$Yellow)) %>%
      add_segments(x = quantile[1], 
                   xend =  quantile[1], y = 0.01, yend = 0.47, showlegend = T, 
                   name ="25th Percentile", line = list(color = palette_reason$SatBlue)) %>%
      
      add_segments(x = quantile[3], 
                   xend =  quantile[3], y = 0.01, yend = 0.47, showlegend = T, 
                   name ="75th Percentile", line = list(color = palette_reason$GreyBlue)) %>%
    
      add_boxplot(x = na.omit(returns_2020[,3]), y =  d1, jitter = 0, pointpos = 0, boxpoints = F,
                  marker = list(color = 'rgb(7,40,89)'),
                  line = list(color = 'rgb(7,40,89)'),
                  name = "All Points") %>%
      
      layout(title = "Probability Distribution of <br> FY2019-20 State Pension Plan Returns",
             xaxis = list(title = "Buckets of FY2019-20 Returns", dtick = 0.01, 
                          tick0 = -0.02, 
                          tickmode = "linear", tickformat = "%",
                          zeroline = FALSE),
             yaxis = list(title = "Share of Pension Plans", tickformat = "%", 
                          range = c(0,0.6),dtick = 0.05, 
                          zeroline = FALSE))%>% 
      layout(annotations= list(annotation))
    #1. Convert to Percentages (both Axis) -- Done
    #2. Add border lines -- Done
    #3. Add normalized probability line -- Done (100*density /sum(density))
    #4. Color -25th, 25th-75th, and 75th+ percentile bars.
    
    #https://rpubs.com/juanhklopper/histograms_plotly_R
    #https://www.r-graph-gallery.com/277-marginal-histogram-for-ggplot2.html
      
    histogram
    
    #Converting X-axis values to % (https://stackoverflow.com/questions/47603495/r-histogram-display-x-axis-ticks-as-percentages)
    
  })
  
}
help(hist)
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
help(rnorm)
