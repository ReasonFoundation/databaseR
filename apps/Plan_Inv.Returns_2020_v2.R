### 2020 Investment Returns ###
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

pl <- planList()
#https://chart-studio.plotly.com/~vigneshbabu/9/_10th-percentile-25th-percentile-median-75th-percentile-90th-percentile/#/
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data, 2001)
reason.data <- reason.data %>% select(year, plan, state,return_1yr, arr)
reason.data <- data.table(reason.data)
reason.data[,4:5] <- reason.data[,4:5] %>% dplyr::mutate_all(dplyr::funs(as.numeric))
reason.data[,1] <- reason.data[,1] %>% dplyr::mutate_all(dplyr::funs(as.numeric))
reason.data <- data.table(reason.data)
years <- seq(2001, 2020, by = 1)
#View(reason.data)
#boxplot(reason.data$return_1yr)

#Creating a vector w/ 10th, 25th, 50th, 75th, and 90th percentiles for 2001
returns.perc <- data.table(quantile(na.omit(reason.data[year == 2001]$return_1yr, c(0.1, 0.25, 0.5, 0.75, 0.9))))
#Creating a Loop to do the same (+binding columns) for 2002-19 years
for (i in (2002:2019)){
  returns.perc <- cbind(returns.perc, data.table(quantile(na.omit(reason.data[year == i]$return_1yr, c(0.1, 0.25, 0.5, 0.75, 0.9)))))
}
#View(returns.perc)
#View(reason.data)

#Adding 2020 returns
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020_v2.csv"
returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
returns_2020$mva_billions <- as.numeric(returns_2020$mva_billions) 
returns_2020$mva_billions_19 <- as.numeric(returns_2020$mva_billions_19) 
returns_2020$arr <- as.numeric(returns_2020$arr) 

returns_2020$`2020_return` <- as.numeric(returns_2020$`2020_return`)
#View(returns.perc)
returns.perc <- cbind(returns.perc, data.table(quantile(na.omit(returns_2020$`2020_return`, c(0.1, 0.25, 0.5, 0.75, 0.9)))))

#Renaming columns + transposing the table
colnames(returns.perc) <- as.character(seq(2001, 2020, by = 1))
returns.perc <- t(returns.perc)
colnames(returns.perc) <- as.character(c("10th", "25th", "50th", "75th", "90th"))
#View(returns.perc)

#Calculating median ARR for 2001-19
#View(reason.data)
arr.perc <- data.table(reason.data[,median(na.omit(arr)), by = list(year)])
#Calculating median ARR for 2020
arr.2020 <- data.table("2020",median(na.omit(returns_2020$arr)))
#View(arr.2020)

arr.perc <- rbind(arr.perc,arr.2020, fill=T)
arr.perc[20,1] <- as.numeric("2020")
arr.perc[20,2] <- as.numeric(median(na.omit(returns_2020$arr)))
arr.perc <- arr.perc[,!3]
#View(arr.perc)
arr.perc$V1 <- as.numeric(arr.perc$V1)

############
######Shiny app[ui] -------------------------------------------------

ui <- fluidPage(
  br(),
  img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("State Public Pension Investment Returns (Updated)"),
  # CODE BELOW: Add select inputs on state and plans to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  
  mainPanel(
    ###Remove error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabsetPanel(
      tabPanel('2020 Investment Returns', width = 5, 
               #radioGroupButtons("filter", "Sort by", 
               #choices = c("Plan Name", "Highest returns", "Assumed Rate of Return", "Asset size"), 
               #selected = c("Plan Name"), status = "primary"),
               DT::DTOutput('plot_Returns'),
               style = "font-size:100%; width:50%"
      ),
      #https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288),
      tabPanel("2020 Return Distribution", 
               switchInput(
                 inputId = "perc",
                 label = "Percentiles", 
                 labelWidth = "70px",
               ),
               plotly::plotlyOutput("plot_Return_Distribution")),
      # tabPanel("2001-20 Return Distribution", 
      #           sliderInput('year', 'Select Starting Year', min = 2001, max = 2020, value = 2001, sep = ""),
      #           prettyRadioButtons("lines", "Add Lines", 
      #           choices = c("None", "Median Assumed Rate of Return (ARR)"), selected = "None", status = "warning"),
      #           plotly::plotlyOutput("plot_InvReturns")),
      
      tags$div(htmlOutput("text1")),
      
      id = "tabset"
    ), 
    id="main"
    
  )
)

##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  output$text1 <- renderText({
    paste(
      "<b>Note</b>: We will be updating our return data regularly, as more state pension plans report their FY2019-20 returns.","<br>", "<br>", 
      HTML(
        "<b>Source</b>:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"),"<br>", 
      "analysis of CAFRs and valuation reports.","<br>", "<br>", 
      "<b>Methodology</b>: 'Estimated Investment Loss' is calculated by
      taking plan's FY2018-19 'Market Value of Assets' and multiplying it by the difference between 'Assumed Rate of Return' and 'FY2019-20 Return'. 
      Estimated values are meant to approximate total amounts of investment loss that plans would fully & directly recognize this year due to FY2019-20 return deviating from the assumption.",
      "Probability Distribution is based on `normalized` probability density function, with all probabilities (i.e. bars) summing up to 100%.", "<br>", "<br>", 
      "*Aggregate state-level data","<br>","**Preliminary returns", sep="\n")
  })
  
  observeEvent(input[["tabset"]], {
    
    hideElement(selector = "#sidebar")
    removeCssClass("main", "col-sm-8")
    addCssClass("main", "col-sm-12")
    
  })
  
  output$plot_Returns <- DT::renderDataTable({
    
    returns_2020 <- data.table(returns_2020)
    
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    returns_2020<- returns_2020 %>% arrange(plan)
    returns_2020<- data.table(returns_2020)
    #Calculating Approximate Investment Loss
    returns_2020[, inv_gain_loss := mva_billions_19*(arr-`2020_return`)]
    returns_2020$inv_gain_loss <- round(returns_2020$inv_gain_loss,3)
    returns_2020$mva_billions <- round(returns_2020$mva_billions,1)
    returns_2020$mva_billions_19 <- as.character(round(returns_2020$mva_billions_19,1))
    colnames(returns_2020) <- c("Pension Plan", "State", "FY19-20 Returns", "Assumed Rate of Return", "Market Assets (Billions)", "2019 Market Assets (Billions)", "Source", "Estimated Investment Loss (Billions)")
    returns_2020[,8] <- round(returns_2020[,8],1)
    returns_2020 <- returns_2020 %>% select("Pension Plan", "State", "FY19-20 Returns", "Assumed Rate of Return", "Market Assets (Billions)", "Estimated Investment Loss (Billions)", "Source")
    returns_2020 <- (returns_2020 %>% arrange(desc(returns_2020[,3])))
    #    if(input$filter == "Highest returns"){
    #     returns_2020 %>% arrange(desc(returns_2020[,3]))
    #  } else if(input$filter == "Asset size"){
    #    returns_2020 %>% arrange(desc(returns_2020[,5]))
    #  } else if(input$filter == "Assumed Rate of Return"){
    #    returns_2020 %>% arrange(desc(returns_2020[,4]))
    #  } else{
    #    returns_2020 %>% arrange(returns_2020[,1])
    #  }
    #  )
    
    
    returns_2020 <- as.data.frame(returns_2020) 
    returns_2020[,3:6] <- returns_2020[,3:6] %>% dplyr::mutate_all(dplyr::funs(as.numeric))
    #View(returns_2020)
    returns_2020 <- data.table(returns_2020)
    
    #Converting web links to active hyperlinks (named "Source") for each plan
    for(i in (1:length(returns_2020$Source))){
      returns_2020$Source[i] <- paste0("<a href=", paste0(returns_2020$Source[i]),">Source</a>")
    }
    
    returns_2020 <- as.data.frame(returns_2020) 
    #for(i in (1:length(na.omit(returns_2020$source)))){
    #  na.omit(returns_2020$source)[i] <- <a href="http://rstudio.com">RStudio</a>)
    #}
    #returns_2020[,7] <- <a href="http://rstudio.com">RStudio</a>
    #http://www.stencilled.me/post/2019-04-18-editable/
    #https://rstudio.github.io/DT/
    returns_2020 <- DT::datatable(returns_2020, escape = FALSE, filter = "top", editable = FALSE, 
                                  options = list(pageLength = 40, autoWidth = TRUE, dom = 't'), rownames= FALSE) %>% 
      #escape = FALSE keeps the a href links
      #filter = "top" add quick search bars below column titles
      #dom = 't' removes list and search bars at the top
      
      formatStyle("FY19-20 Returns", 
                  #background = styleColorBar(returns_2020[,3], palette_reason$LightBlue),
                  #backgroundSize = '98% 88%',
                  #backgroundRepeat = 'no-repeat',
                  #backgroundPosition = 'right',
                  fontWeight = styleEqual(10, "bold")) %>%
      
      formatPercentage(c("FY19-20 Returns", "Assumed Rate of Return"),2) %>%
      
      formatCurrency(c("Market Assets (Billions)", "Estimated Investment Loss (Billions)"))
    
    #set to TRUE to allow editing
    
    #returns_2020 <- as.data.table(returns_2020)
    #returns_2020[,one_year_gain_loss := (returns_2020$mva_billions * (returns_2020$arr - returns_2020$`2020_return`))]
  })
  
  output$plot_Return_Distribution <- plotly::renderPlotly({
    
    urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020_v2.csv"
    returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
    returns_2020$mva_billions <- as.numeric(returns_2020$mva_billions) 
    returns_2020$mva_billions_19 <- as.numeric(returns_2020$mva_billions_19) 
    returns_2020$arr <- as.numeric(returns_2020$arr) 
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    
    returns_2020<- returns_2020 %>% arrange(plan)
    
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
    
    annotation <- list(yref = 'paper', xref = "x", y = 0.5, x = median(round(na.omit(returns_2020[,3]),2)), text = paste("Median = ", round(median(na.omit(returns_2020[,3]))*100,1), "%"))
    #https://plotly.com/chart-studio-help/histogram/
    d1 <- density(na.omit(returns_2020[,3]), bw = "SJ")
    
    quantile <- quantile(na.omit(returns_2020[,3]), c(0.25, 0.5, 0.75))
    
    histogram <- plot_ly(x = na.omit(returns_2020[,3]),
                         textposition = "outside",
                         type = "histogram",
                         histnorm = "probability", 
                         showlegend = FALSE,
                         nbinsx = 12, 
                         marker = list(color = palette_reason$LightOrange,
                                       line = list(color = "black",
                                                   width = 1)),
    ) %>% 
      
      #add_trace(x = d1$x,
      #          y = 100*(d1$y/sum(d1$y)),
      #          type="scatter",mode = 'line+markers', fill = 'toze roy', showlegend = F) %>%
      
      # add_trace(x = fit$x, y = fit$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
      #
      
      #add_boxplot(x = na.omit(returns_2020[,3]), y =  d1, jitter = 0, pointpos = 0, boxpoints = F,
      #            marker = list(color = 'rgb(7,40,89)'),
      #            line = list(color = 'rgb(7,40,89)'),
    #            name = "All Points") %>%
    
    layout(title = "Probability Distribution of <br> FY2019-20 State Pension Plan Returns",
           xaxis = list(title = "Range of FY2019-20 Returns", dtick = 0.01, 
                        tick0 = -0.02, 
                        tickmode = "linear", tickformat = "%",
                        range = c(-0.05,0.11),
                        zeroline = FALSE),
           yaxis = list(title = "Proportion of Pension Plans (out of 100%)", tickformat = "%", 
                        range = c(0,0.45),dtick = 0.05, 
                        zeroline = FALSE)) %>% 
      
      layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
                                y = 0.35, x = 0, text = "reason.org/pensions",
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=9, color="black")))  %>%
      layout(autosize = T) %>%
      layout(hovermode="unified", 
             if((sum(input$perc) > 0) == T){annotations= list(annotation)})
    
    #Adding ercentiles per user input
    if((sum(input$perc) > 0) == T){
      histogram <- histogram %>% 
        add_segments(x = quantile[1], 
                     xend = quantile[1], y = 0.005, yend = 0.4, showlegend = T, 
                     name = paste0("25th Percentile (", round(quantile[1]*100,1), "%)"), line = list(color = palette_reason$SatBlue)) %>%
        
        add_segments(x = quantile[2], 
                     xend = quantile[2], y = 0.005, yend = 0.4, showlegend = T, 
                     name = paste0("Median (", round(quantile[2]*100,1), "%)"), line = list(color = palette_reason$Yellow)) %>%
        
        add_segments(x = quantile[3], 
                     xend = quantile[3], y = 0.005, yend = 0.4, showlegend = T, 
                     name = paste0("75th Percentile (", round(quantile[3]*100,1), "%)"), line = list(color = palette_reason$Green)) 
      
    }
    
    
    #1. Convert to Percentages (both Axis) -- Done
    #2. Add border lines -- Done
    #3. Add normalized probability line -- Done (100*density /sum(density))
    #4. Color -25th, 25th-75th, and 75th+ percentile bars.
    
    #https://rpubs.com/juanhklopper/histograms_plotly_R
    #https://www.r-graph-gallery.com/277-marginal-histogram-for-ggplot2.html
    
    histogram
    
    #Converting X-axis values to % (https://stackoverflow.com/questions/47603495/r-histogram-display-x-axis-ticks-as-percentages)
    
  })
  
  #output$plot_InvReturns <- plotly::renderPlotly({
  #  reason.data <- data.table(reason.data)
  #  years <- seq(min(input$year,2018), 2020, by = 1)
  #  
  #  #boxplot(reason.data$return_1yr)
  #  
  #  #Creating a vector w/ 10th, 25th, 50th, 75th, and 90th percentiles for 2001
  #  returns.perc <- data.table(quantile(na.omit(reason.data[year == min(input$year,2018)]$return_1yr, c(0.1, 0.25, 0.5, 0.75, 0.9))))
  #  #Creating a Loop to do the same (+binding columns) for 2002-19 years
  #  for (i in ((min(input$year,2018) + 1):2019)){
  #    returns.perc <- cbind(returns.perc, data.table(quantile(na.omit(reason.data[year == i]$return_1yr, c(0.1, 0.25, 0.5, 0.75, 0.9)))))
  #  }
  #  
  #  #View(returns.perc)
  #  
  #  #Adding 2020 returns
  #  urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020.csv"
  #  returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
  #  returns_2020$mva_billions <- as.numeric(returns_2020$mva_billions) 
  #  returns_2020$mva_billions_19 <- as.numeric(returns_2020$mva_billions_19) 
  #  returns_2020$arr <- as.numeric(returns_2020$arr) 
  #  returns_2020$`2020_return` <- as.numeric(returns_2020$`2020_return`)
  #  #View(returns.perc)
  #  returns.perc <- cbind(returns.perc, data.table(quantile(na.omit(returns_2020$`2020_return`, c(0.1, 0.25, 0.5, 0.75, 0.9)))))
  #  returns.perc <- data.frame(returns.perc )
  #  returns.perc <- returns.perc %>% dplyr::mutate_all(dplyr::funs(as.numeric))
  #  returns.perc <- data.table(returns.perc)
  #  #Renaming columns + transposing the table
  #  colnames(returns.perc) <- as.character(seq(min(input$year,2018), 2020, by = 1))
  #  returns.perc <- t(returns.perc)
  #  colnames(returns.perc) <- as.character(c("10th", "25th", "50th", "75th", "90th"))
  #  
  #  ##########
  #  
  #  trace1 <- list(
  #    uid = "2983cb", 
  #    fill = "none", 
  #    line = list(
  #      color = palette_reason$Orange, 
  #      shape = "spline", 
  #      width = 1, 
  #      smoothing = 0.6
  #    ), 
  #    mode = "lines", 
  #    name = "10th Percentile", 
  #    x = years, 
  #    y = round(returns.perc[,1],2), 
  #    fillcolor = "rgba(159, 197, 232, 0.63)", 
  #    connectgaps = FALSE
  #  )
  #  trace2 <- list(
  #    uid = "324d0c", 
  #    fill = "tonexty", 
  #    line = list(
  #      color = palette_reason$Red, 
  #      shape = "spline", 
  #      width = 1, 
  #      smoothing = 0.6
  #    ), 
  #    mode = "lines", 
  #    name = "25th Percentile", 
  #    x = years, 
  #    y = round(returns.perc[,2],2), 
  #    fillcolor = "rgba(159, 197, 232, 0.63)", 
  #    connectgaps = FALSE
  #  )
  #  trace3 <- list(
  #    uid = "a3a908", 
  #    fill = "tonexty", 
  #    line = list(
  #      color = "rgb(31, 119, 180)", 
  #      shape = "spline", 
  #      smoothing = 0.6
  #    ), 
  #    mode = "lines", 
  #    name = "Median", 
  #    x = years, 
  #    y = round(returns.perc[,3],2), 
  #    fillcolor = "rgba(31, 119, 180, 0.5)", 
  #    connectgaps = FALSE
  #  )
  #  trace4 <- list(
  #    uid = "fdf1b8", 
  #    fill = "tonexty", 
  #    line = list(
  #      color = palette_reason$Red, 
  #      shape = "spline", 
  #      width = 1, 
  #      smoothing = 0.6
  #    ), 
  #    mode = "lines", 
  #    name = "75th Percentile", 
  #    x = years, 
  #    y = round(returns.perc[,4],2),   
  #    fillcolor = "rgba(31, 119, 180, 0.5)", 
  #    connectgaps = FALSE
  #  )
  #  trace5 <- list(
  #    uid = "9ae1f4", 
  #    fill = "tonexty", 
  #    line = list(
  #      color = palette_reason$Orange, 
  #      shape = "spline", 
  #      width = 1, 
  #      smoothing = 0.6
  #      
  #    ), 
  #    mode = "lines", 
  #    name = "90th Percentile", 
  #    x = years, 
  #    y = round(returns.perc[,5],2), 
  #    fillcolor = "rgba(159, 197, 232, 0.63)", 
  #    connectgaps = FALSE
  #  )
  #  
  #  data <- list(trace1, trace2, trace3, trace4, trace5)
  #  layout <- list(
  #    xaxis = list(
  #      type = "category", 
  #      range = c(0.0, 95.0), 
  #      showgrid = TRUE, 
  #      zeroline = TRUE, 
  #      autorange = TRUE, 
  #      gridwidth = 1
  #    ), 
  #    yaxis = list(
  #      type = "linear", 
  #      range = c(-0.30, 0.30), 
  #      showgrid = TRUE, 
  #     zeroline = TRUE, 
  #      autorange = TRUE, 
  #      gridwidth = 1
  #   ), 
  #   autosize = TRUE
  #  )
  #  p <- plot_ly()
  #  p <- add_trace(p, uid=trace1$uid, fill=trace1$fill, line=trace1$line, mode=trace1$mode, name=trace1$name, 
  #                 type=trace1$type, x=trace1$x, y=round(trace1$y,2), connectgaps=trace1$connectgaps)
  #  p <- add_trace(p, uid=trace2$uid, fill=trace2$fill, line=trace2$line, mode=trace2$mode, name=trace2$name, 
  #                type=trace2$type, x=trace2$x, y=round(trace2$y,2), fillcolor=trace2$fillcolor, connectgaps=trace2$connectgaps)
  #  p <- add_trace(p, uid=trace3$uid, fill=trace3$fill, line=trace3$line, mode=trace3$mode, name=trace3$name, 
  #                 type=trace3$type, x=trace3$x, y=round(trace3$y,2), fillcolor=trace3$fillcolor, connectgaps=trace3$connectgaps)
  # p <- add_trace(p, uid=trace4$uid, fill=trace4$fill, line=trace4$line, mode=trace4$mode, name=trace4$name, 
  #                 type=trace4$type, x=trace4$x, y=round(trace4$y,2), fillcolor=trace4$fillcolor, connectgaps=trace4$connectgaps)
  #  p <- add_trace(p, uid=trace5$uid, fill=trace5$fill, line=trace5$line, mode=trace5$mode, name=trace5$name, 
  #                 type=trace5$type, x=trace5$x, y=round(trace5$y,2), fillcolor=trace5$fillcolor, connectgaps=trace5$connectgaps)
  #  
  #if(input$lines %in% "Zero"){p <- add_segments(p, x = years, 
  #                  xend =  2020, y = 0, yend = 0, showlegend = F,
  #                  name = "", line = list(color ="black"))
  #}
  
  #https://rstudio.github.io/DT/010-style.html
  #https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
  
  #  if(input$lines == "Median Assumed Rate of Return (ARR)"){
  #    p <- add_lines(p, x = trace5$x, 
  #            xend =  max(trace5$x), y = round(arr.perc[year >= min(input$year,2018)]$V1,2), 
  #            yend = last(round(arr.perc[year >= min(input$year,2018)]$V1,2)), showlegend = T,
  #            name = "Median ARR", line = list(color = palette_reason$LightOrange))
  #  }
  #  
  #  p <- layout(p, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, height=layout$height, autosize=layout$autosize)
  #  p <- layout(p, yaxis = list(title = "Market Valued Returns (Actual)", dtick = 0.03, tick0 = -0.3, 
  #                              tickmode = "linear", tickformat = "%", zeroline = FALSE)) %>%
  #    layout(hovermode = 'compare') %>%
  #    layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
  #                              y = 0.01, x = 2018, text = "reason.org/pensions",
  #                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
  #                              font=list(size=9, color="black"))) # setting "compare" hover option as a default
  #  
  #})
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)