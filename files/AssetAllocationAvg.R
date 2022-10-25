# Asset Allocation App
# ### Data: PPD
# rm(list=ls())
# 
#renv::init() 
#renv::restore()
# #https://github.com/rstudio/renv
# 
# #library(extrafont)
# #font_import()
# #loadfonts()
# 
#devtools::install_github(
#  "ReasonFoundation/pensionviewr_private",
#   ref = "master", auth_token = "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",
#  force = TRUE
#  )
# 
# ###Load/install packages
# 
# #To install `reasontheme` & `pensionviewr` packages you first want to load `devtools`
# #install.packages('devtools')
# #library(devtools)
# #devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
# #
# #
# #devtools::install_github(
# #  "ReasonFoundation/pensionviewr_private",
# #  auth_token = "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",
# #  force = TRUE
# #)
# 
# #devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
# #devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
# library(reasontheme)
# library(pensionviewr)
#library(janitor)
#remotes::install_github("rstudio/renv")
library(grid)
library(tidyverse)
library(openxlsx)
library(tseries)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(base64enc)
#Shiny
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(plotly)
library(rlang)
library(purrr)
library(rpart)
library(vip)
library(gridExtra)


pl <- planList()

######### pullAssetData() #########
###################################

pullAssetData <- function (fy, mva = FALSE)
{

  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")

  query <- paste("select * from pull_data_state_allocations()\nwhere year > '",
                 paste(fy - 1), "'",if(isTRUE(mva)){paste(",\n'Market Value of Assets Dollar'")})


  result <- RPostgres::dbSendQuery(con, query)
  all_data <- RPostgres::dbFetch(result) %>% janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  all_data %>% dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>% dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value) %>%
    dplyr::select(-.data$row_id) %>% dplyr::arrange(display_name,
                                                    year) %>% janitor::clean_names()
}


assets <- pullAssetData(fy=2001, mva = FALSE)

#######
filterAssetsAvg <- function(data){

assets <- data.frame(data)
assets[,5:83]  <- assets[,5:83] %>% mutate_all(as.numeric)
assets <- assets %>% select(year,contains("percent"))%>%
  replace(is.na(.), 0)

##############
assets2 <- assets %>%
    group_by(year) %>%
    mutate(percent_of_assets_invested_in_misc_alternatives =
             percent_of_assets_invested_in_misc_alternatives +
           percent_of_assets_invested_in_other_investments) %>%
    summarise(
      across(
        c(colnames(assets[,2:10])),
        .fns = list(
          "mean" = ~ mean(.x)
        )),
      .groups = "drop") %>%

    select(-percent_of_assets_invested_in_other_investments_mean)

##############
assets2 <- assets2 %>% select(year,
                            percent_of_assets_invested_in_private_equity_mean,
                            percent_of_assets_invested_in_real_estate_mean,
                            percent_of_assets_invested_in_hedge_funds_mean,
                            percent_of_assets_invested_in_commodities_mean,
                            percent_of_assets_invested_in_misc_alternatives_mean,
                            percent_of_assets_invested_in_equity_investments_total_mean,
                            percent_of_assets_invested_in_fixed_income_mean,
                            percent_of_assets_invested_in_cash_mean)

##############
colnames(assets2) <- c("year", "Private Equity",
                      "Real Estate", "Hedge Funds", "Commodities", "Other Alternatives",
                      "Public Equity", "Fixed Income", "Cash")
assets2
}
# ######
assets <- filterAssetsAvg(assets)
#assets <- assets %>% filter(year < 2021)
#View(assets)
#write.csv(assets, "assets.csv")

v <- c(0, 0, 1, 1)

x <- pullStateData(2001)
lapply(x, range)
#assets <- data.frame(read.csv('/Users/anilniraula/databaseR/assets.csv'))
assets <- assets %>% select(-X)

# assets$total <- 0
# y <- as.numeric(length(assets$total))
# assets[, 2:9] <- assets[, 2:9] %>% mutate_all(as.numeric)
# for (i in (1:y)) {
#   assets$total[i] <- sum(assets[i, 2:9])
# }
# 
assets[21,2] <- assets[21,2] + (1-sum(assets[21,2:9]))
View(assets)
#write.csv(assets, "assets.csv")

length(rep(8,88))
x <- c(1, 2, 3)
rownames(x) <- c("one", "two", "three")
#View(sum(round(assets[,2:9]*100,1)))
######
######
palette_reason <- list(Orange="#FF6633",
                             LightOrange="#FF9900",
                             DarkGrey="#333333", 
                             LightGrey= "#CCCCCC", 
                             SpaceGrey ="#A69FA1",
                             DarkBlue="#0066CC", 
                             GreyBlue= "#6699CC", 
                             Yellow= "#FFCC33",
                             LightBlue = "#66B2FF", 
                             SatBlue = "#3366CC", 
                             Green = "#669900",
                             LightGreen = "#00CC66", 
                             Red = "#CC0000",
                             LightRed="#FF0000")
######
plotTheme <- ggplot2::theme(    panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                plot.margin = margin(0.5,1,0,0, "cm"),
                                legend.key.height=unit(1,"line"),
                                legend.margin = margin(0,0,0,0, "cm"),
                                axis.text.y = element_text(size=12, color = "black"),
                                axis.text.x = element_text(size=12, color = "black", angle = 0, hjust = 0.5, vjust = -1),
                                axis.title = element_text(size = 12, face = "bold"),
                                legend.text = element_text(size = 12),
                                legend.title = element_blank(),
                                text = element_text(family = "Calibri"))
######
######

assetPlot <- function (data, interactive = FALSE) 
{
  data <- data.table(data)
  
  alloc <- data.table(melt(data, id.vars = "year"))
  alloc$value <- as.numeric(alloc$value)
  alloc$year <- as.numeric(alloc$year)
  alloc$value <- round(alloc$value * 100, 1)
  alloc <- data.frame(alloc)
 
  colors <- c(palette_reason$Red, palette_reason$LightRed, 
              palette_reason$Orange, palette_reason$LightOrange, palette_reason$Yellow, 
              palette_reason$DarkGrey, palette_reason$SatBlue, palette_reason$LightBlue)
  
  
  graph <- ggplot(alloc, aes(x = year, y = value, fill = variable, 
                             group = variable, text = paste0("Fiscal Year: ", year, 
                                                             "<br>", "Class: ", variable, "<br>", "Allocation: ", 
                                                             value, "%"))) + geom_area(position = "stack", stat = "identity") + 
    scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x, "%"), 
                       name = "% of Total Portfolio", expand = c(0,0)) + scale_fill_manual(values = colors) + 
    ggplot2::scale_x_continuous(breaks = seq(min(data$year), 
                                             max(data$year), by = 2), expand = c(0, 0))+
    theme_bw() + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(), 
                       panel.border = element_blank(),  text = element_text(family = "Calibri"),
                       axis.title.x = element_blank(), 
                       legend.key.height=unit(1,"line"),
                       legend.margin = margin(0,0,0,0, "cm"),
                       axis.line = element_line(colour = "black"), 
                       legend.title = element_blank(),
                       legend.position="bottom")+
    plotTheme
   
      graph <- ggplotly(graph, tooltip = "text")
  
  graph
}


ui <- fluidPage(
  titlePanel("Average Asseta Allcation by State-Managed Pension Plans (2001-2021)"),
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
       tabPanel("Asset Ret/Alloc",
                 plotly::plotlyOutput("plot_Allocation"))
      )
    )
  )

##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  
  output$plot_Allocation <- plotly::renderPlotly({
    
    graph <- assetPlot(assets, interactive = FALSE)
    
    graph <- ggplotly(graph)
    
    graph
    ###########
  })
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
##########################