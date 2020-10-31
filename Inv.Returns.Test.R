rm(list=ls())

ui <- fluidPage(
  titlePanel("Return Distribution"),
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(width = 0),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel('Return Distribution', plotly::plotlyOutput("plot_Return_Distribution"))),
    )
  )
)
##########################

Shiny app[server] -------------------------------------------------
  
  server <- function(input, output, session){
    
    output$plot_Return_Distribution <- plotly::renderPlotly({
      
      returns_2020 <- c(0.005, 0.024, 0.039, 0.047, 0.028, 0.0084, 0.0236, 0.0357, 0.072, -0.01, 0.019, 0.045, 0.013, 0.0335, 0.038)
      
      annotation <- list(yref = 'paper', xref = "x", y = 0.5, x = mean(na.omit(returns_2020)), text = paste("Mean = ", round(mean(na.omit(returns_2020))*100,1), "%"))
      
      histogram <- plot_ly(x = ~na.omit(returns_2020),
                           type = "histogram",
                           histnorm = "probability",
                           nbinsx = input$bins, 
                           marker = list(color = "orange",
                                         line = list(color = "black",
                                                     width = 2))) %>%
        
        add_segments(x = mean(na.omit(returns_2020)),
                     xend = mean(na.omit(returns_2020)), y = 0.01, yend = 0.47, showlegend = FALSE) %>%
        #add_lines(x = na.omit(returns_2020[,3]), y = density(na.omit(returns_2020[,3]))) %>% # add a density estimate with defa
        
        layout(title = "Probability Distribution of Investment Returns",
               xaxis = list(title = "Return Buckets", dtick = 0.01,
                            tick0 = -0.02,
                            tickmode = "linear", tickformat = "%",
                            zeroline = FALSE),
               yaxis = list(title = "Share of Pension Plans", tickformat = "%",
                            range = c(0,0.5),dtick = 0.05,
                            zeroline = FALSE))%>%
        layout(annotations= list(annotation))
      
      histogram
      
    })
  }
shinyApp(ui = ui, server = server)
#####`