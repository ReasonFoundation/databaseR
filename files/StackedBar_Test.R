######################
######################
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
library(ggplot2)
library(tidyverse)
library(tseries)
library(data.table)
library(readr)
library(rsconnect)
library(dplyr)
library(plyr)
library(plotly)

###Create Data
data <- data.table(

  Plan.Cost = c(1050,760,652),
  Social.Security = c(689, 689, 689),
  Scenario = c("Do it Yourself","QLAC","T100% Annuity"),#arranged by alphabet order -> Adding "T" to 100% Annuity (can remove in png)
  Text = c("Scenario 1<br><br>Plan", "Scenario 2<br><br>Plan + QLAC<br>(28% Less in Accruals)", "Scenario 3<br><br>Plan / Immediate Annuity<br>(38% Less in Accruals)")
  
)

colnames(data)[1:2] <- c("Plan Accumulation", "Social Security")
#View(data)
###Transform to Long format
data <- data.table(melt(data, by = c("Plan.Cost")))
data <- data.frame(data %>% arange(-Scenario))

data$Text[4:6] <- "SS"

#######
#######

#data$value <- scales::dollar(data$value)
#View(data)

######## Create Stacked Bar Chart
#1. Set up theme & colors
#2. Gggplot

plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0.5, 0.5,0,0, "cm"),
                               axis.text.y = element_text(size=12, color = "black"),
                               axis.text.x = element_text(size=12, color = "black", angle = 0, hjust = 0.5, vjust = -1),
                               axis.title = element_text(size = 12, face = "bold"),
                               legend.text = element_text(size = 12),
                               legend.title = element_blank(),
                               text = element_text(family = "Calibri"))


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
                       Green = "#669900",LightGreen = "#00CC66", Red = "#CC0000",LightRed="#FF0000")


######## Ggplot
bars <- ggplot(data, aes(y = value, fill = variable)) + 
  geom_col(aes(x = Scenario))+theme_bw() + 
  theme_classic() +
  plotTheme +
  
  scale_fill_manual(values=c(palette_reason$SatBlue,palette_reason$Orange))+
  
  scale_y_continuous(
    labels = function(x) paste0("$", x), 
    name = "Net Present Value of Benefits ($Thousands)",
    breaks = seq(0, 1800, by = 200), limits = c(0, 1800),
    expand=c(0,0))+
  
  geom_text(aes(x = Scenario, label = paste0("$", value, "K", "<br>", Text)),
            colour = "white", 
            size = 4,
            vjust = 1,
            position = position_stack(0.5))

ggplotly(bars)

###############
