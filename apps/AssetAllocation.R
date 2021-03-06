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

assets <- pullAssetData(2001)
assets[,5:74]  <- assets[,5:74] %>% mutate_all(as.numeric)
#View(assets[,75])## TOTAL

## Convert NA's to 0
assets <- assets %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(across(where(is.numeric))))

#y <- as.numeric(length(assets$total))

#for (i in (1: y)){
#  assets$total[i] <- sum(assets[i,5:75])  
#}
assets <- assets[,1:74]
assets$year <- as.numeric(assets$year)

#for (i in (1: y)){
#  assets$total[i] <- sum(assets[i,5:75])  
#}

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
#View(other)
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

assets2 <- assets %>% select(year, display_name, state, colnames(assets[,75:82]))
#assets2$total <- 0
#View(assets2) 
#y <- as.numeric(length(assets2$total))

assets[,4:11]  <- assets[,4:11] %>% mutate_all(as.numeric)

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
alloc$value <- round(alloc$value*100,1)
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
               
               plotly::plotlyOutput("plot_Allocation"))
  )
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
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
