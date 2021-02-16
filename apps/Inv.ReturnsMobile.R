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

pl <- planList()
#https://chart-studio.plotly.com/~vigneshbabu/9/_10th-percentile-25th-percentile-median-75th-percentile-90th-percentile/#/

############
######Shiny app[ui] -------------------------------------------------

ui <- fluidPage(
  br(),
  #img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("State Public Pension Investment Return Results"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
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
                 labelWidth = "70px"
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
    paste("<br>","<br>",
          "<b>Notes</b>: This table and chart were last updated on 12/14/2020 to reflect 84 of 114 major state-managed pension plans reporting their FY19-20 investment return results.","<br>", "<br>",  
          "Each bar of the Return Distribution chart shows the percentage of state plans with officially reported results that fall within each two percent investment return range." ,"<br>", "<br>",
          #HTML(
          #"<b>Source</b>:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"),"<br>", 
          #"analysis of CAFRs and valuation reports.","<br>", "<br>", 
          "<b>Methodology</b>: 'Estimated Investment Loss (Billions)' is calculated by
      taking plan's FY2018-19 'Market Value of Assets' (Defined Benefit retirement funds) and multiplying it by the difference between 'Assumed Rate of Return' and 'FY2019-20 Return'. 
      Estimated values are meant to approximate total amounts of investment loss that plans would fully & directly recognize this year due to FY2019-20 return deviating from the assumption.",
          "'Asset-Weighted Return' is calculated by weighting FY2019-20 retuns by each plans' portion of the 2019 total market value of assets. Investment returns shown are Net of Fees, if not stated otherwise.",
          "Probability Distribution is based on `normalized` probability density function, with all probabilities (i.e. bars) summing up to 100%.", "<br>", "<br>", 
          "*Includes aggregate state-level data","<br>","**Preliminary returns","<br>", "***Gross of fees", sep="\n")
  })
  
  observeEvent(input[["tabset"]], {
    
    hideElement(selector = "#sidebar")
    removeCssClass("main", "col-sm-8")
    addCssClass("main", "col-sm-12")
    
  })
  
  output$plot_Returns <- DT::renderDataTable({
    
    #Adding 2020 returns
    urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020_v2.csv"
    returns_2020 <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
    
    returns_2020 <- data.table(returns_2020)
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    returns_2020 <- returns_2020 %>% arrange(plan)
    #Calculating Approximate Investment Loss
    returns_2020 <- data.table(returns_2020)
    returns_2020[, inv_gain_loss := -(mva_billions_19*(arr-`2020_return`))]
    returns_2020$inv_gain_loss <- round(as.numeric(returns_2020$inv_gain_loss),1)
    returns_2020$mva_billions <- round(as.numeric(returns_2020$mva_billions),1)
    returns_2020$mva_billions_19 <- round(as.numeric(returns_2020$mva_billions_19),1)
    colnames(returns_2020) <- c("Pension Plan", "State", "FY19-20 Return", "Assumed Rate of Return", "Market Assets (Billions)", "2019 Market Assets (Billions)", "Source", "Estimated Investment Loss (Billions)")
    returns_2020[,8] <- round(returns_2020[,8],1)
    returns_2020 <- returns_2020 %>% select("Pension Plan", "State", "FY19-20 Return", "Assumed Rate of Return", "Market Assets (Billions)", "Estimated Investment Loss (Billions)", "Source")
    returns_2020 <- (returns_2020 %>% arrange(returns_2020[,2]))
    
    #View(returns_2020)
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
    returns_2020 <- data.table(returns_2020)
    #View(returns_2020)
    #Converting web links to active hyperlinks (named "Source") for each plan
    
    for(i in (1:length(returns_2020$Source))){
      returns_2020$Source[i] <- paste0("<a href=", paste0(returns_2020$Source[i])," target='_blank'",">Source</a>")
    }
    
    returns_2020 <- as.data.frame(returns_2020) 
    #for(i in (1:length(na.omit(returns_2020$source)))){
    #  na.omit(returns_2020$source)[i] <- <a href="http://rstudio.com">RStudio</a>)
    #}
    #returns_2020[,7] <- <a href="http://rstudio.com">RStudio</a>a
    #http://www.stencilled.me/post/2019-04-18-editable/
    #https://rstudio.github.io/DT/
    #View(returns_2020)
    returns_2020 <- DT::datatable(returns_2020, escape = FALSE, editable = FALSE, rownames= FALSE,
                                  
                                  options = list(
                                    dom = 'Bftp',
                                    pageLength = 30, info = FALSE,
                                    autoWidth = TRUE#
                                    #columnDefs = list(list(searchable = FALSE, targets = 7))
                                    #columnDefs = list(list(targets=c(2:5), visible=TRUE, width='10px'))
                                    #,scrollX = "20%"
                                  )) %>% 
      
      #escape = FALSE keeps the a href links
      #filter = "top" add quick search bars below column titles
      #dom = 't' removes list and search bars at the top
      
      formatStyle("FY19-20 Return", 
                  #background = styleColorBar(returns_2020[,3], palette_reason$LightBlue),
                  #backgroundSize = '98% 88%',
                  #backgroundRepeat = 'no-repeat',
                  #backgroundPosition = 'right',
                  fontWeight = styleEqual(10, "bold")) %>%
      
      formatPercentage(columns = c("FY19-20 Return"),digits = 1) %>%
      formatPercentage(columns = c("Assumed Rate of Return"),digits = 2) %>%
      
      formatCurrency(columns = c("Market Assets (Billions)", "Estimated Investment Loss (Billions)"),digits = 1)
    
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
    returns_2020$`2020_return` <- returns_2020$`2020_return`
    returns_2020 <- returns_2020 %>% filter(!is.na(returns_2020$`2020_return`))
    returns_2020 <- returns_2020 %>% filter(state != "Pennsylvania")
    returns_2020 <- returns_2020 %>% arrange(plan)
    # View(returns_2020)
    
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
                         nbinsx = 13, 
                         marker = list(color = palette_reason$LightOrange,
                                       line = list(color = "black", width = 1))) %>% 
      
      #https://mran.revolutionanalytics.com/snapshot/2016-03-14/web/packages/plotly/plotly.pdf
      #config(displayModeBar = FALSE)%>%
      
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
      
      ## Annotations
      # layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
      #              y = 0.4, x = 0, text = "reason.org/pensions",
      #             xanchor='right', yanchor='auto', xshift=0, yshift=0,
      #              font=list(size=9, color="black")))  %>%
      
      layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
                                y = 0.49, x = 0, text = paste("Asset-Weighted", "<br>", "Return = ", round(sum(na.omit(returns_2020[,3]*returns_2020[,6]))/sum(na.omit(returns_2020[,6])),3)*100, "%"),
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=11, color="black")))  %>%
      
      # layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
      #                      y = 0.8, x = 0.08, text = paste(
      #                        "Given the economic and market volatility", "<br>", 
      #                        "spurred by the COVID-19 crisis, many    ", "<br>", 
      #                        "expected U.S. public pension plans’     ", "<br>", 
      #                        "investment returns in FY2019-20 to fall ", "<br>", 
      #                        "well into a negative territory. Yet, per", "<br>", 
      #                        "our analysis of reported returns so far ", "<br>", 
      #                        "show that yields are mainly fall within a", "<br>", 
      #                        "positive 1%-4% range, with median   ", "<br>", 
    #                        "at 2.8%, and 25th and 75th percentiles  ", "<br>", 
    #                        "at 1.2% and 4.0% returns, respectively. ", "<br>", "<br>", 
    #                        "Each Histogram bar shows probability    ", "<br>", 
    #                        "of state plans reporting returns within ", "<br>", 
    #                        "each 2 percentage point range on X-axis.", "<br>", 
    #                        "Aternatively, bars show proportion of   ", "<br>", 
    #                        "plans with returns within these ranges. ", "<br>", 
    #                        "All bar probabilities stack up to 100%. "
    #                      ),
    #                      xanchor='left', yanchor='top', xshift=0, yshift=0,
    #                      font=list(size=10, color="black")))  %>%
    ## Margins + hovermode
    layout(autosize = T, 
           margin = list(l = 30, r = 60, t = 60, b = 10)) %>%
      
      layout(autosize = T) %>%
      layout(hovermode="unified", 
             if((sum(input$perc) > 0) == T){annotations= list(annotation)})
    
    ## Adding percentiles per user turning button ON
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
    histogram <- plotly::config(histogram, displayModeBar = FALSE)
    
    histogram
    
    #Converting X-axis values to % (https://stackoverflow.com/questions/47603495/r-histogram-display-x-axis-ticks-as-percentages)
    
  })
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)