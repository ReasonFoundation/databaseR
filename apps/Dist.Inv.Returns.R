## State Investment Returns (2001-20)
# by Anil Niraula
#Data: 2001-19 Reason database, 2020-manually collected

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


#Pulling state datat from the database
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data, 2001)
reason.data <- reason.data %>% select(year, plan_name, state,return_1yr)
reason.data$return_1yr <- as.numeric(reason.data$return_1yr)
reason.data$year <- as.numeric(reason.data$year)
reason.data <- data.table(reason.data)
years <- seq(2001, 2020, by = 1)

#boxplot(reason.data$return_1yr)

#Creating a vector w/ 10th, 25th, 50th, 75th, and 90th percentiles for 2001
returns.perc <- data.table(quantile(na.omit(reason.data[year == 2001]$return_1yr, c(0.1, 0.25, 0.5, 0.75, 0.9))))
#Creating a Loop to do the same (+binding columns) for 2002-19 years
for (i in (2002:2019)){
returns.perc <- cbind(returns.perc, data.table(quantile(na.omit(reason.data[year == i]$return_1yr, c(0.1, 0.25, 0.5, 0.75, 0.9)))))
}
#View(returns.perc)

#Adding 2020 returns
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/Plan_Inv.Returns_2020.csv"
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

############

ui <- fluidPage(
  titlePanel("Distribution of State Pension Investment Returns (2001-2020)"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 50),
                 
    ),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel("Returns", plotly::plotlyOutput("plot_InvReturns")),
        tags$div(HTML(paste("<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>"))),
        tags$div(htmlOutput("text1")))
      )
    )
  )

##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){

############

                           
# Get this figure: fig <- get_figure("vigneshbabu", 9)
# Get this figure's data: data <- get_figure("vigneshbabu", 9)$data
# Add data to this figure: p <- add_trace(p, x=c(4, 5), y=c(4, 5), kwargs=list(filename="Ambulatory Glucose Profile", fileopt="extend"))
# Get y data of first trace: y1 <- get_figure("vigneshbabu", 9)$data[[1]]$y

# Get figure documentation: https://plotly.com/r/get-requests/
# Add data documentation: https://plotly.com/r/file-options/

# You can reproduce this figure in R with the following code!

# Learn about API authentication here: https://plotly.com/r/getting-started
# Find your api_key here: https://plotly.com/settings/api

  output$text1 <- renderText({
    paste(HTML(
      "<b>Source</b>:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"),"<br>", 
      "analysis of U.S. public pension actuarial valuation reports, CAFRs, and publicly available investment statements.", "<br>", "<br>", 
      "<b>Methodology and Notes</b>:", "Percentiles represent the percentage/proability of investment returns for that year falling at that rate or below.", "<br>", 
      "Some plans are yet to disclose 2019 data. Distribution for 2020 is based on returns so far repported by around 20% of major state pension plans.", sep="\n")
  })
  
output$plot_InvReturns <- plotly::renderPlotly({
trace1 <- list(
  uid = "2983cb", 
  fill = "none", 
  line = list(
    shape = "spline", 
    width = 0, 
    smoothing = 0.6
  ), 
  mode = "lines", 
  name = "10th Percentile", 
  type = "scatter", 
  x = years, 
  y = returns.perc[,1], 
  connectgaps = FALSE
)
trace2 <- list(
  uid = "324d0c", 
  fill = "tonexty", 
  line = list(
    shape = "spline", 
    width = 0, 
    smoothing = 0.6
  ), 
  mode = "lines", 
  name = "25th Percentile", 
  type = "scatter", 
  x = years, 
  y = returns.perc[,2], 
  fillcolor = "rgba(159, 197, 232, 0.63)", 
  connectgaps = FALSE
)
trace3 <- list(
  uid = "a3a908", 
  fill = "tonexty", 
  line = list(
    color = "rgb(31, 119, 180)", 
    shape = "spline", 
    smoothing = 0.6
  ), 
  mode = "lines", 
  name = "Median", 
  type = "scatter", 
  x = years, 
  y = returns.perc[,3], 
  fillcolor = "rgba(31, 119, 180, 0.5)", 
  connectgaps = FALSE
)
trace4 <- list(
  uid = "fdf1b8", 
  fill = "tonexty", 
  line = list(
    shape = "spline", 
    width = 0, 
    smoothing = 0.6
  ), 
  mode = "lines", 
  name = "75th Percentile", 
  type = "scatter", 
  x = years, 
  y = returns.perc[,4],   
  fillcolor = "rgba(31, 119, 180, 0.5)", 
  connectgaps = FALSE
)
trace5 <- list(
  uid = "9ae1f4", 
  fill = "tonexty", 
  line = list(
    shape = "spline", 
    width = 0, 
    smoothing = 0.6
  ), 
  mode = "lines", 
  name = "90th Percentile", 
  type = "scatter", 
  x = years, 
  y = returns.perc[,5], 
  fillcolor = "rgba(159, 197, 232, 0.63)", 
  connectgaps = FALSE
)

data <- list(trace1, trace2, trace3, trace4, trace5)
layout <- list(
  width = 1100, 
  xaxis = list(
    type = "category", 
    range = c(0, 95), 
    showgrid = TRUE, 
    zeroline = TRUE, 
    autorange = TRUE, 
    gridwidth = 1
  ), 
  yaxis = list(
    type = "linear", 
    range = c(-0.3, 0.3), 
    showgrid = TRUE, 
    zeroline = TRUE, 
    autorange = TRUE, 
    gridwidth = 1
  ), 
  height = 590, 
  autosize = TRUE
)
p <- plot_ly()
p <- add_trace(p, uid=trace1$uid, fill=trace1$fill, line=trace1$line, mode=trace1$mode, name=trace1$name, 
               type=trace1$type, x=trace1$x, y=trace1$y, connectgaps=trace1$connectgaps)
p <- add_trace(p, uid=trace2$uid, fill=trace2$fill, line=trace2$line, mode=trace2$mode, name=trace2$name, 
               type=trace2$type, x=trace2$x, y=trace2$y, fillcolor=trace2$fillcolor, connectgaps=trace2$connectgaps)
p <- add_trace(p, uid=trace3$uid, fill=trace3$fill, line=trace3$line, mode=trace3$mode, name=trace3$name, 
               type=trace3$type, x=trace3$x, y=trace3$y, fillcolor=trace3$fillcolor, connectgaps=trace3$connectgaps)
p <- add_trace(p, uid=trace4$uid, fill=trace4$fill, line=trace4$line, mode=trace4$mode, name=trace4$name, 
               type=trace4$type, x=trace4$x, y=trace4$y, fillcolor=trace4$fillcolor, connectgaps=trace4$connectgaps)
p <- add_trace(p, uid=trace5$uid, fill=trace5$fill, line=trace5$line, mode=trace5$mode, name=trace5$name, 
               type=trace5$type, x=trace5$x, y=trace5$y, fillcolor=trace5$fillcolor, connectgaps=trace5$connectgaps)
p <- layout(p, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, height=layout$height, autosize=layout$autosize)
p <- layout(p, yaxis = list(title = "FY2001-20 Actual Investment Returns", dtick = 0.02, tick0 = -0.3, 
                            tickmode = "linear", tickformat = "%", zeroline = FALSE)) %>%
  layout(hovermode = 'compare')# setting "compare" hover option as a default

})
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
###########
