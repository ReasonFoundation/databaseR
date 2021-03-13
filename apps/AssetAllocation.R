### Asset Allocation App
### Data: PPD
rm(list=ls())

###Load/install packages

#To install `reasontheme` & `pensionviewr` packages you first want to load `devtools`
#install.packages('devtools')
#library(devtools)

#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(grid)
library(tidyverse)
#library(openxlsx)
library(tseries)
library(plyr)
library(dplyr)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny
library(shiny)
library(shinyWidgets)
#library(shinyFiles)
library(DT)
library(plotly)
library(rlang)
library(purrr)
library(rpart)
library(vip)

pl <- planList()

######### pullAssetData() #########
pullAssetData <- function (FY) 
{
  
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  query <- paste("select * from pull_data_state_only()\nwhere year > '", 
                 paste(FY - 1), "'\nand attribute_name in ('International Equity Actual Allocation',
\n'Domestic Equity Actual Allocation',
\n'Fixed Income + Alternatives Actual Allocation',
\n'Real Estate Actual Allocation',
\n'Domestic Fixed Income Actual Allocation',
\n'International Fixed Income Actual Allocation',
\n'Private Equity Actual Allocation',
\n'Absolute Return/Value Fund Actual Allocation',
\n'Farmland/Agriculture Actual Allocation',
\n'High Yield Fixed Income Actual Allocation',
\n'Fixed Income Actual Allocation',
\n'Cash/Short-term Actual Allocation',
\n'Inflation Linked Bonds/TIPS Actual Allocation',
\n'Misc. Equity Actual Allocation',
\n'Other” Actual Allocation',
\n'Large Cap Equity Actual Allocation',
\n'Micro Cap Equity Actual Allocation',
\n'Multi-Asset Class/Diversified Actual Allocation',
\n'REIT Actual Allocation',
\n'Real Assets/Real Return Actual Allocation',
\n'Credit Opportunities/Private Credit Actual Allocation',
\n'GTAA/GAA/Global Macro Hedge Actual Allocation',
\n'Risk Parity Actual Allocation',
\n'Opportunistic Equity Alternative Actual Allocation',
\n'Opportunistic Debt Alernatives Actual Allocation',
\n'Emerging Fixed Income Actual Allocation',
\n'Private Debt Actual Allocation',
\n'Equity Securities Lending Actual Allocation',
\n'Infrastructure Actual Allocation',
\n'MLP/Limited Partnership Actual Allocation',
\n'Investment Grade Fixed Income Actual Allocation',
\n'Loans Actual Allocation',
\n'Large Cap Domestic Equity Actual Allocation',
\n'Small Cap Domestic Equity Actual Allocation',
\n'Private Real Estate Actual Allocation',
\n'Timberland Actual Allocation',
\n'Hedge Funds Actual Allocation',
\n'Fixed Income Mortgages/Securitized Debt Actual Allocation',
\n'Inflation-Sensititive Actual Allocation',
\n'Global Fixed Income Actual Allocation',
\n'Private Placement Actual Allocation',
\n'Global Equity Actual Allocation',
\n'Core Fixed Income Actual Allocation',
\n'Opportunity/Opportunistic Actual Allocation',
\n'Commodities Actual Allocation',
\n'Core Real Estate Actual Allocation',
\n'Emerging International Equity Actual Allocation',
\n'Developed International Equity Actual Allocation',
\n'Relative Return/Value Fund Actual Allocation',
\n'US Treasury Actual Allocation',
\n'Covered Call Actual Allocation',
\n'Natural Resources Actual Allocation',
\n'Non-Core Fixed Income Actual Allocation',
\n'Opportunistic Fixed Income Actual Allocation',
\n'Corporate Bonds Actual Allocation',
\n'Misc. Alternatives Actual Allocation',
\n'Mid Cap Domestic Equity Actual Allocation',
\n'Small Cap Equity” Actual Allocation',
\n'Equity Hedge Actual Allocation',
\n'Non-Core Real Estate Actual Allocation',
\n'Fixed Income Funds of Funds Actual Allocation',
\n'Value Added Fixed Income Actual Allocation',
\n'Passive International Equity Actual Allocation',
\n'Active International Equity Actual Allocation',
\n'Core Equity Actual Allocation',
\n'Global Growth Equity Actual Allocation',
\n'Triple Net Lease Actual Allocation',
\n'Distressed Debt Actual Allocation',
\n'Nominal Fixed Income Actual Allocation',
\n'Convertible Fixed Income Actual Allocation',
\n'Fixed Income ETI Actual Allocation',
\n'Structured Fixed Income Actual Allocation',
\n'Fixed Income Below Investment Grade Actual Allocation',
\n'Fixed Income + Cash Actual Allocation')")
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
##################

pullReturnData <- function (FY) 
{
  
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  query <- paste("select * from pull_data_state_only()\nwhere year > '", 
                 paste(FY - 1), "'\nand attribute_name in ('International Equity Actual Allocation',
\n'Domestic Equity Return',
\n'Fixed Income + Alternatives Return',
\n'Real Estate Return',
\n'Domestic Fixed Income Return',
\n'International Fixed Income Return',
\n'Private Equity Return',
\n'Absolute Return/Value Fund Return',
\n'Farmland/Agriculture Return',
\n'High Yield Fixed Income Return',
\n'Fixed Income Return',
\n'Cash/Short-term Return',
\n'Inflation Linked Bonds/TIPS Return',
\n'Misc. Equity Return',
\n'Other” Return',
\n'Large Cap Equity Return',
\n'Micro Cap Equity Return',
\n'Multi-Asset Class/Diversified Return',
\n'REIT Return',
\n'Real Assets/Real Return Return',
\n'Credit Opportunities/Private Credit Return',
\n'GTAA/GAA/Global Macro Hedge Return',
\n'Risk Parity Return',
\n'Opportunistic Equity Alternative Return',
\n'Opportunistic Debt Alernatives Return',
\n'Emerging Fixed Income Return',
\n'Private Debt Return',
\n'Equity Securities Lending Return',
\n'Infrastructure Return',
\n'MLP/Limited Partnership Return',
\n'Investment Grade Fixed Income Return',
\n'Loans Return',
\n'Large Cap Domestic Equity Return',
\n'Small Cap Domestic Equity Return',
\n'Private Real Estate Return',
\n'Timberland Return',
\n'Hedge Funds Return',
\n'Fixed Income Mortgages/Securitized Debt Return',
\n'Inflation-Sensititive Return',
\n'Global Fixed Income Return',
\n'Private Placement Return',
\n'Global Equity Return',
\n'Core Fixed Income Return',
\n'Opportunity/Opportunistic Return',
\n'Commodities Return',
\n'Core Real Estate Return',
\n'Emerging International Equity Return',
\n'Developed International Equity Return',
\n'Relative Return/Value Fund Return',
\n'US Treasury Return',
\n'Covered Call Return',
\n'Natural Resources Return',
\n'Non-Core Fixed Income Return',
\n'Opportunistic Fixed Income Return',
\n'Corporate Bonds Return',
\n'Misc. Alternatives Return',
\n'Mid Cap Domestic Equity Return',
\n'Small Cap Equity” Return',
\n'Equity Hedge Return',
\n'Non-Core Real Estate Return',
\n'Fixed Income Funds of Funds Return',
\n'Value Added Fixed Income Return',
\n'Passive International Equity Return',
\n'Active International Equity Return',
\n'Core Equity Return',
\n'Global Growth Equity Return',
\n'Triple Net Lease Return',
\n'Distressed Debt Return',
\n'Nominal Fixed Income Return',
\n'Convertible Fixed Income Return',
\n'Fixed Income ETI Return',
\n'Structured Fixed Income Return',
\n'Fixed Income Below Investment Grade Return',
\n'Fixed Income + Cash Return')")
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
###################
assets <- pullAssetData(2001)
#View(assets)

assets[,5:74]  <- assets[,5:74] %>% mutate_all(as.numeric)
#View(assets[,75])## TOTAL
#View(reason.data$unfunded_actuarially_accrued_liabilities_dollar)

## Convert NA's to 0
assets <- assets %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(across(where(is.numeric))))

#y <- as.numeric(length(assets$total))

#for (i in (1: y)){
#  assets$total[i] <- sum(assets[i,5:75])  
#}

assets$year <- as.numeric(assets$year)

#for (i in (1: y)){
#  assets$total[i] <- sum(assets[i,5:75])  
#}
#View(assets)

equity <- data.frame(assets %>% select(contains("equity"), 
                                        -contains("private")))
fixed <- data.frame(assets %>% select(contains("fixed")))
private.equity <- data.frame(assets %>% select(contains("private")))
real.estate <- data.frame(assets %>% select(contains("estate")))
cash <- data.frame(assets %>% select(contains("short")))
infrastructure <- data.frame(assets %>% select(contains("infrastructure")))
hedge <- data.frame(assets %>% select(contains("hedge"),
                                       -contains("equity")))
other <- data.frame(assets %>% select(-contains("equity"),
                                       -contains("fixed"),
                                       -contains("private"),
                                       -contains("estate"),
                                       -contains("short"),
                                       -contains("infrastructure"),
                                       -contains("year"),
                                       -contains("plan_id"),
                                       -contains("display_name"),
                                       -contains("state"),
                                       -contains("hedge")))
other <- other[,1:22]
#View(colnames(assets))
#View(equity)

assets$private.equity <- 0
assets$real.estate <- 0
assets$hedge <- 0
assets$infrastructure <- 0
assets$other.alternatives <- 0
assets$equity <- 0
assets$fixed <- 0
assets$cash <- 0

for(i in (1:length(assets$equity))){
  
  assets$private.equity[i] <- sum(private.equity[i,])
  assets$real.estate[i] <- sum(real.estate[i,])
  assets$hedge[i] <- sum(hedge[i,])
  assets$infrastructure[i] <- sum(infrastructure[i,])
  assets$other.alternatives[i] <- sum(other[i,])
  assets$equity[i] <- sum(equity[i,])
  assets$fixed[i] <- sum(fixed[i,])
  assets$cash[i] <- sum(cash[i,])
  
}
#View((assets[,76:83]))
assets2 <- assets %>% select(year, display_name, state, colnames(assets[,76:83]))
#assets2$total <- 0
#View(assets2) 
#y <- as.numeric(length(assets2$total))

assets2[,4:11]  <- assets2[,4:11] %>% mutate_all(as.numeric)

#for (i in (1: y)){
#  assets2$total[i] <- sum(assets2[i,4:10])  
#}

#View(assets2)
#View(assets2)
## Calcualte Means for eacha sset class
average.allocation <- assets2 %>%
  group_by(year) %>%
  summarise(
    across(
      c(colnames(assets2[4:11])),  
      .fns = list(
        "mean" = ~ mean(.x)
      )),
    .groups = "drop")

alloc <- as.data.table(average.allocation)
colnames(alloc) <- c("year",
                   "Private Equity",
                   "Real Estate",
                   "Hedge Funds",
                   "Infrastructure",
                   "Other Alternatives",
                   "Total Equity",
                   "Fixed Income",
                   "Cash Equivalents")

alloc <- data.table(melt(alloc, id.vars = "year"))
alloc$value <- as.numeric(alloc$value)
alloc$value <- round(alloc$value*100,1)

###################
returns <- pullReturnData(2001)

returns[,5:71]  <- returns[,5:71] %>% mutate_all(as.numeric)
#View(assets[,75])## TOTAL
#View(reason.data$unfunded_actuarially_accrued_liabilities_dollar)

## Convert NA's to 0
returns <- returns %>%
  replace(is.na(.), 0)

#y <- as.numeric(length(assets$total))

#for (i in (1: y)){
#  assets$total[i] <- sum(assets[i,5:75])  
#}

returns$year <- as.numeric(returns$year)

#for (i in (1: y)){
#  assets$total[i] <- sum(assets[i,5:75])  
#}
#View(returns)

equity <- data.frame(returns %>% select(contains("equity"), 
                                       -contains("private")))
fixed <- data.frame(returns %>% select(contains("fixed")))
private.equity <- data.frame(returns %>% select(contains("private")))
real.estate <- data.frame(returns %>% select(contains("estate")))
cash <- data.frame(returns %>% select(contains("short")))
infrastructure <- data.frame(returns %>% select(contains("infrastructure")))
hedge <- data.frame(returns %>% select(contains("hedge"),
                                      -contains("equity")))
other <- data.frame(returns %>% select(-contains("equity"),
                                      -contains("fixed"),
                                      -contains("private"),
                                      -contains("estate"),
                                      -contains("short"),
                                      -contains("infrastructure"),
                                      -contains("year"),
                                      -contains("plan_id"),
                                      -contains("display_name"),
                                      -contains("state"),
                                      -contains("hedge")))

returns$private.equity <- 0
returns$real.estate <- 0
returns$hedge <- 0
returns$infrastructure <- 0
returns$other.alternatives <- 0
returns$equity <- 0
returns$fixed <- 0
returns$cash <- 0


for(i in (1:length(returns$equity))){

  returns$private.equity[i] <- mean(private.equity[i,])
  returns$real.estate[i] <- mean(real.estate[i,])
  returns$hedge[i] <- mean(hedge[i,])
  returns$infrastructure[i] <- mean(infrastructure[i,])
  returns$other.alternatives[i] <- mean(other[i,])
  returns$equity[i] <- mean(equity[i,])
  returns$fixed[i] <- mean(fixed[i,])
  returns$cash[i] <- mean(cash[i,])
  
}
## Convert NA's to 0
returns <- returns %>%
  replace(is.na(.))

returns2 <- returns %>% select(year, display_name, state, colnames(returns[,72:79]))
#assets2$total <- 0
#View(assets2) 
#y <- as.numeric(length(assets2$total))

returns2[,4:11]  <- returns2[,4:11] %>% mutate_all(as.numeric)
#View(returns2)
#for (i in (1: y)){
#  assets2$total[i] <- sum(assets2[i,4:10])  
#}

#View(assets2)
#View(assets2)
## Calcualte Means for eacha sset class
average.allocation <- returns2 %>%
  group_by(year) %>%
  summarise(
    across(
      c(colnames(returns2[4:11])),  
      .fns = list(
        "mean" = ~ mean(.x)
      )),
    .groups = "drop")

ret <- as.data.table(average.allocation)
colnames(ret) <- c("year",
                     "Private Equity",
                     "Real Estate",
                     "Hedge Funds",
                     "Infrastructure",
                     "Other Alternatives",
                     "Total Equity",
                     "Fixed Income",
                     "Cash Equivalents")

ret <- data.table(melt(ret, id.vars = "year"))
ret$value <- as.numeric(ret$value)
ret$value <- round(ret$value*100,1)

#View(alloc)

############
############

ui <- fluidPage(
  br(),
  img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ANiraula/PublicPlansData/master/reason_logo.png"), width = 200, height = 55),
  titlePanel("Average Asset Allocation by State-Managed Plans (2001-2020)"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  
  mainPanel(
    ###Remove error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabsetPanel(
      #https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288),
      tabPanel("Year-By-Year Asset Alocation", 
               
               plotly::plotlyOutput("plot_Allocation")),
      
      tabPanel("Year-By-Year Asset Returns", 
               
               plotly::plotlyOutput("plot_Returns"))),
  
)
)


##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
 
  output$plot_Allocation <- plotly::renderPlotly({
colors <- c(palette_reason$Red,
            palette_reason$LightRed,
            palette_reason$Orange,
            palette_reason$LightOrange,
            palette_reason$Yellow,
            palette_reason$DarkGrey,
            palette_reason$SatBlue,
            palette_reason$LightBlue)

graph <- ggplot(alloc,aes(x = year, y = value, fill = variable)) +
  geom_bar(
                      #text = paste0("Fiscal Year: ", year, "<br>",
                                    #"Class: ",variable, "<br>",
                                   # "Allocation: ",round(value *100,2), "%")), 
                      cposition="stack", stat="identity")+
  scale_y_continuous(labels = function(x) paste0(x,"%"), name = "% of Total Portfolio")+
  scale_fill_manual(values=colors)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    plot.title = element_text(family = "Arial", size = 14, margin=margin(0,0,0,0)),
    axis.title.y = element_text(family = "Arial", size = 12, margin=margin(0,0,0,0))
  )#+ theme(legend.position = "none")

graph <- ggplotly(graph)
graph <- graph  %>% layout(height = 550)

graph
###########
})
  
  output$plot_Returns <- plotly::renderPlotly({
    colors <- c(palette_reason$Red,
                palette_reason$LightRed,
                palette_reason$Orange,
                palette_reason$LightOrange,
                palette_reason$Yellow,
                palette_reason$DarkGrey,
                palette_reason$SatBlue,
                palette_reason$LightBlue)
    
    graph <- ggplot(ret,aes(x = year, y = value, color = variable)) +
      geom_line(
        #text = paste0("Fiscal Year: ", year, "<br>",
        #"Class: ",variable, "<br>",
        # "Allocation: ",round(value *100,2), "%")), 
        #cposition="stack", stat="identity"
        )+
      scale_y_continuous(labels = function(x) paste0(x,"%"), name = "Average Annual Return (by Asset Class)")+
      scale_color_manual(values=colors)+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        plot.title = element_text(family = "Arial", size = 14, margin=margin(0,0,0,0)),
        axis.title.y = element_text(family = "Arial", size = 12, margin=margin(0,0,0,0))
      )#+ theme(legend.position = "none")
    
    graph <- ggplotly(graph)
    graph <- graph  %>% layout(height = 550)
    
    graph
    ###########
  })
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
