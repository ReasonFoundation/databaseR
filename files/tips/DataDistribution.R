#Download Quarterly GDP by State from BEA
##==by Anil
#==Release: https://bea.gov/newsreleases/regional/gdp_state/qgdpstate_newsrelease.htm
#==Tables only: https://bea.gov/newsreleases/regional/gdp_state/2018/xls/qgdpstate0518.xlsx
#Packages required: XLConnect, fitdistrplus

#Extracting downloaded and slightly pre-processed GDP by state data from BEA (2017, Q4)
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
library(plyr)
library(tseries)
library(openxlsx)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
library(shinymanager)
library(repmis)
library(plotly)
#library(shinyFiles)
#Other-----------
library(XLConnect)
library(fitdistrplus)


#Sources:
#https://www.investopedia.com/articles/06/probabilitydistribution.asp

##Loading State GDP data from GitHub:
#https://github.com/ReasonFoundation/databaseR/tree/master/files/tips
urlfile <- "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/tips/qgdpstate0518_truncated.csv"
GDPSt <- data.table(
  read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
)

#GDPSt = readWorksheetFromFile(url, header = FALSE,
#                              sheet = 4, startRow = 4, startCol = 1, endCol = 17, endRow = 65)
View(GDPSt)

#Excluding regional totals
#("New England", "Midwest", "Great Lakes", "Plains", 
#"Southeast", "Southwest", "Rocky Mountain", and "Far West")
StateGDP <-as.data.frame(GDPSt[-c(8, 15, 21, 29, 42, 47, 53, 60:61),c(1,9)]) #convert GDP to $Millions
StateGDP <- StateGDP %>%
  select(StateGDP[,1], StateGDP[,9]) %>%
  arrange(desc(StateGDP[,1]))

colnames(StateGDP) <- c("State", "GDP_State")
StateGDP$GDP_State <- as.numeric(StateGDP$GDP_State)/1000
View(StateGDP)

#Number of non-zero observations
n = sum(!is.na(StateGDP))
n

## Summary Statistic
View(StateGDP %>%
       summarize(mean = round(mean(as.matrix(StateGDP$GDP_State)),1),
                 median = round(median(as.matrix(StateGDP$GDP_State)),1),
                 sd = round(sd(as.matrix(StateGDP$GDP_State)),1)))

#as.data.frame(replicate(100,sample(StateGDP)))
#sample(StateGDP,1, replace=T)

#Law of large numbers (resampling + regression to the mean)
mean(as.matrix(StateGDP$GDP_State))
mean(rnorm(100, mean=mean(as.matrix(StateGDP$GDP_State)), sd=sd(as.matrix(StateGDP$GDP_State))))

summary(StateGDP$GDP_State)
#GDP by State    
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#32.59   96.67  231.46  397.39  517.31 2802.29 

#Considerable difference between mean and median
#can show that the data might not be normally distributed

#To plot the data we need to convert it back to "numeric" class
GSPSt <- as.matrix(StateGDP$GDP_State)
GSP <- as.numeric(GSPSt)
#View(GSP)
#Plot the data, CDF, and density 
#(historgams work best for simple visualization of underlying distribution)
hist(GSP)
## Using log10 transformation to spread the distribution (for visualization)
hist(log10(GSP))
#Plot CDF (i.e. cumulative density function for continuous variables)
plot(ecdf(GSP))
#https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/02%3A_Descriptive_Statistics/2.05%3A_The_Empirical_Rule_and_Chebyshev%27s_Theorem

#Density is another relevant way to show where data points are concentrated
plot(density(GSP))
abline(v = mean(GSP), col = "green") #drawing a vertical line for the average GDP
abline(v = median(GSP), col = "blue") #drawing a vertical line for median GDP
abline(v = mean(GSP)+sd(GSP), col = "lightblue") #drawing a vertical line that is 1 standard deviation from the average GDP
abline(v = mean(GSP)-sd(GSP), col = "orange")

#Creating a binary vector of states with GDP +- one standard deviation
St.dev <- ifelse(GSP>=mean(GSP)-sd(GSP) & GSP<=mean(GSP)+sd(GSP),1,0)
mean(GSP)-sd(GSP)
mean(GSP)+sd(GSP)
table(St.dev)# 47 out of 51 states are within this range
sum(St.dev)/length(St.dev) # 0.9038462 or 90%. Thus, 10% or 5 states are outside this range
#Empirical rule of thumb: Chebyshev's Theorem = 1−1/k^2 (k = number of standard deviations)
1-1/3^2# At least 89% of all not normally distributed data should be with 3 sd of the mean
#Boxplot is another handy tool to spot outliers, it shows 1st, 2nd (median), and 3rd Quartiles
#as well as maximum and minimum

#Outliers could potentially be spotted by looking at data points that are 1.5×IQR (interquartile range = Q3-Q1)
#away from either the third or the first quartile.
glimpse(boxplot(StateGDP$GDP_State))
#View(StateGDP)
#California, Texas, and New York have the highest GDP (potential outliers), followed by Florida.

#Excluding these three states for a prompt visual
sample1 <- StateGDP %>% 
  filter(State != "New York", State != "Texas", State !="California")

plot(ecdf(Sample1))

#Identify kinds of distribution that fits the data (need library "fitdistrplus")
#Run the comparative distribution analysis
descdist(GSP, discrete = FALSE)
#Major types of distribution:
#"norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif" and "logis"
#Fit couple of distributions
#Try fit normal distribution
fit.norm <- fitdist(GSP, "norm")
plot(fit.norm)
fit.norm$aic #Akaike information criterion shows relative quality of statistical models for a given dataset
#It is only useful when comparing models to each other, as AIC estimator of out-of-sample prediction error.
#The purpose woudl be to find model that minimizes the information loss (i.e. has the minimum AIC)
#Try fit lognormal distribution
fit.lognorm <- fitdist(GSP, "lnorm")
plot(fit.lognorm)
fit.lognorm$aic
#Try fit exponential distribution
fit.exp <- fitdist(GSP, "exp")
plot(fit.exp)
fit.exp$aic
#Try fit logistic distribution
fit.logis <- fitdist(GSP, "logis")
plot(fit.logis)
fit.logis$aic

#Given the AIC values for the four distributions, our data is best represented by lognormal distribution, 
#folowed by exponential, logistic, and then by normal distributions
#https://en.wikipedia.org/wiki/Log-normal_distribution