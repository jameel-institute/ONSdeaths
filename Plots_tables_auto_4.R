### This script plots the figures and tables using
### 1) the cleaned up ONS all-cause mortality data (CleanUP_1.R)
### 2) cleaned up ONS COVID-19 mortality data (Covid_ONS_CleanUP_1.R)
### 3) the forecasts of expected all-cause deaths and weekly cumulative (ExcessDeathsCalculations_2.R)
###
### The script plots the forecast since 2019 ("full forecast") and since March 2020 by region and age/sex,
### and generates tables with the cumulative deaths (all-cause, expected, COVID-19 and excess non-COVID-19) 
### up to the most recent date.

suppressPackageStartupMessages(library(tidyverse))
library(readxl)
suppressPackageStartupMessages(library(lubridate))
library(matrixStats)
options(scipen=9999)
library(KFAS)
library(stargazer)
library(rtiff)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(reshape)
library(lemon)
library(ggpubr)

# Function to clean the data further
file <- "./CleanedDataForEstimation.csv"
cleancleaneddata <- function() {
  data<-read.csv(file,stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended,"%Y-%m-%d")
  data$last.week.year<-0
  data$year<-format(data$Week.ended,"%Y")
  data[(data$Week.number==52 & data$year %in% c(2010,2011,2012,2013,2014,2016,2017,2018,2019)),'last.week.year']<-1
  data[data$Week.number==53,'last.week.year']<-1
  pos<-which(data$last.week.year ==1)
  
  for (i in pos){
  x1<-data[data$X==i, 4:35]
  x2<-data[data$X==i+1, 4:35]
  x<-(x1+x2)/2
  data[data$X==i, 4:35]<-x
  data[data$X==i+1, 4:35]<-x
}
  end1<-ymd("2020-03-01")
  n<-length(data$Week.ended[data$Week.ended>=end1])
  # data$respiratory<-rowSums(data[,c('ICD.10.v.2010',
  #                                 'All.respiratory.diseases..ICD.10.J00.J99...ICD.10.v.2010',
  #                                 'All.respiratory.diseases..ICD.10.J00.J99..ICD.10.v.2013..IRIS.',
  #                                 'ICD.10.v.2010..NCHS.')], na.rm=T)
  data$England<-rowSums(data[,c('North.East','North.West','Yorkshire.and.The.Humber','East.Midlands','West.Midlands',
                              'East', 'London', 'South.East', 'South.West')], na.rm = TRUE)  
  data<-data[,c( "Week.number",                                                                          
               "Week.ended",                                                                           
               "Total.deaths..all.ages",                                                               
               "All.Under.1.year",                                                                     
               "All.01.14",                                                                            
               "All.15.44" ,                                                                           
               "All.45.64",                                                                            
               "All.65.74" ,                                                                           
               "All.75.84",                                                                            
               "All.85.",                                                                              
               "Male.Under.1.year",                                                                    
               "Male.01.14" ,                                                                          
               "Male.15.44" ,                                                                          
               "Male.45.64" ,                                                                          
               "Male.65.74" ,                                                                          
               "Male.75.84",                                                                           
               "Male.85.",                                                                             
               "Female.Under.1.year",                                                                  
               "Female.01.14",                                                                         
               "Female.15.44",                                                                         
               "Female.45.64",                                                                         
               "Female.65.74",                                                                         
               "Female.75.84",                                                                         
               "Female.85." ,                                                                          
               "North.East",                                                                           
               "North.West",                                                                           
               "Yorkshire.and.The.Humber",                                                             
               "East.Midlands",                                                                        
               "West.Midlands",                                                                        
               "East",                                                                                 
               "London",                                                                               
               "South.East" ,                                                                          
               "South.West",                                                                           
               "Wales", 
               "England")]#,
               #"respiratory")]
  return(data)
}
cleandat <- cleancleaneddata()

# Function to extract ggplot legend for gridarrange
g_legend <- function(a.gplot){ # function for legend grob
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Function to extract  and clean forecasts
ExtractData<-function(i){
  data<-read.csv(paste0("./forecasts/",i),stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  data <- data[data$Week.ended<=as.Date(max(cleandat$Week.ended, na.rm = TRUE)),] # until most recent date updated
  data[,sub(".csv","",i)] <- cleandat[,sub(".csv", "",i)]
  
  data<-data[,c('Week.ended','fit','lwr','upr','signal',sub(".csv", "",i))]
  colnames(data)<-c('Week.ended','fit','lwr','upr','signal','deaths')
  
  temp1<-read.csv("./Covid_Registration.csv")
  if (i == "England.csv") {
    temp1 <- temp1 %>%
      mutate(England = rowSums(dplyr::select(., North.East,North.West,Yorkshire.and.The.Humber,East.Midlands,West.Midlands,
                                             East, London, South.East, South.West), na.rm = TRUE))
  }
  temp1$Week.ended<-as.Date(temp1$Week.ended, origin = "1899-12-30")
  temp1[temp1 == 0] <- NA
  temp1<-temp1[,c('Week.ended',sub(".csv", "",i))]
  colnames(temp1)<-c('Week.ended','covid')
  data <- data[data$Week.ended<=as.Date(max(temp1$Week.ended)),] # until most recent date updated
  data<-merge(data,temp1[,],by.x='Week.ended',by.y='Week.ended',all.x=TRUE)
  data$region<-sub(".csv", "",i)
  data$covid[is.na(data$covid)] <- 0
  return(data)}

# Function to plot total registered all-cause deaths, predicted deaths (from 2010 to February 2020), forecasted
# deaths (from March 2020 onwards), and excess deaths (registered all-cause deaths - expected - COVID-19 deaths).
# This function can be toggled to plot these since January 2019 (full forecast) or since COVID-19 deaths 
# (March 2020 onwards)
pp<-function(i){ 
  data<-read.csv(paste0("./forecasts/",i),
                 stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  data<-data[,c('Week.ended','fit','lwr','upr','signal',sub(".csv", "",i))]
  colnames(data)<-c('Week.ended','fit','lwr','upr','signal','var') 
  data <- data[data$Week.ended<=as.Date(max(cleandat$Week.ended, na.rm = TRUE)),] 
  data$var <- cleandat[,sub(".csv", "",i)]
  
  temp1<-read.csv("./Covid_Registration.csv")
  temp1[temp1 == 0] <- NA
  temp1$Week.ended<-as.Date(temp1$Week.ended, origin = "1899-12-30")
  
  if (i == "England.csv") {
    temp1 <- temp1 %>%
      mutate(England = rowSums(dplyr::select(., North.East,North.West,Yorkshire.and.The.Humber,East.Midlands,West.Midlands,
                                East, London, South.East, South.West), na.rm = TRUE))
  }
  
  temp1<-temp1[,c('Week.ended',sub(".csv", "",i))]
  colnames(temp1)<-c('Week.ended','covid')
  
  
  ## Toggle for full forecast / March onwards ###################################################################
  data <- data[data$Week.ended>=as.Date("2020-03-01"),] # dates for March onwards; comment out for full forecast
  #data <- data[data$Week.ended>=as.Date("2019-07-01"),] # dates for full forecast; comment out for March onwards
  ###############################################################################################################
  
  
  data<-merge(data,temp1[,],by.x='Week.ended',by.y='Week.ended',all.x=TRUE)
  data$var_covid <- data$var - data$covid
  data$covid <- NULL
  mdf <- reshape2::melt(data, id.var = c("Week.ended", "lwr", "upr"))
  
  
  ## Toggle for full forecast / March onwards ###################################################################
  mdf <- mdf[!(mdf$variable == "signal"),] # for March onwards; comment this line out for full forecast
  mdf$variable <- factor(mdf$variable, levels=c("fit","var","var_covid"),#, "signal"), # take out "signal" for March onwards
                         labels=c(paste("Forecasted 06/03-", format(max(mdf$Week.ended), "%d/%m"), sep = ""),
                                  "Observed all-cause","Registered non-COVID deaths")) #, "Predicted" take out "Predicted" for March onwards
  #################################################################################################################
  
  
  
  p<-ggplot(mdf, aes(x = Week.ended, y = value, colour = variable))+
    geom_ribbon(aes(ymin = lwr, ymax = upr), colour = NA, fill = "grey70",alpha=0.5)+
    geom_line() + 
    
    ## Toggle for full forecast / March onwards ##################################################################
    scale_x_date(breaks=unique(mdf$Week.ended)[c(TRUE,FALSE, FALSE)],date_labels = "%d/%m") + # for March onwards; comment out for full forecast
    #scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%m/%y", date_breaks = "1 month") + # for full forecast; comment out for March onwards
    ##############################################################################################################
    
    theme_bw(base_size = 12, base_family = "Helvetica")+ theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(gsub("[.]"," ",sub(".csv", "",i)))+
    
    ## Toggle for full forecast / March onwards ##################################################################
    scale_colour_manual(values = c("Black","Red", "Purple")) + #, "Blue" take out "Blue" for March onwards
    ##############################################################################################################
  
    labs(colour = "Deaths", y ="Deaths", x = "")
  
  return(p)
}

##### PLOTS ###### -----------------------------------------------------------------------------------------------
# Get legend from full forecast/March onwards ggplot
legendgrob <- g_legend(pp("Yorkshire.and.The.Humber.csv") + theme(legend.position = "bottom"))

# Plot of England & Wales from March onwards
pdf("Deaths_EW_MarOnwards.pdf", width = 10, height = 4)
grid.arrange(pp("England.csv") + theme(legend.position = "none"), pp("Wales.csv") +theme(legend.position = "none"),
             bottom = legendgrob, ncol=2) 
dev.off()

# Plot regions of England
pdf("Regional_forecasts_England_fullforecast.pdf", width = 10, height = 7) #_MarOnwards.pdf
grid.arrange(
  pp("North.West.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("North.East.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("Yorkshire.and.The.Humber.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("West.Midlands.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("East.Midlands.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("East.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("South.West.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("London.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("South.East.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  bottom = legendgrob, ncol=3) 
dev.off()

# Plot older age groups (all sexes) 
pdf("Deaths_Allolder_MarOnwards.pdf", width = 8, height = 5) #_fullforecast.pdf
grid.arrange(pp("All.45.64.csv")+theme(legend.position = "none")+ggtitle("All 45-64 yrs"),
             pp("All.65.74.csv")+theme(legend.position = "none")+ggtitle("All 65-74 yrs"),
             pp("All.75.84.csv")+theme(legend.position = "none")+ggtitle("All 75-84 yrs"),
             pp("All.85..csv")+theme(legend.position = "none")+ggtitle("All 85+ yrs"),
             bottom = legendgrob, ncol=2) 
dev.off()

# Plot older age groups with full forecast/March onwards
pdf("Deaths_AllolderAllsex_MarOnwards.pdf", width = 15, height = 5) #_fullforecast.pdf
grid.arrange(
  ## Full forecast order of plots with pdf dimensions width = 10, height = 7
  # pp("All.85..csv")+theme(legend.position = "none")+ggtitle("All 85+ yrs"),
  # pp("Female.85..csv")+theme(legend.position = "none")+ggtitle("Female 85+ yrs"),
  # pp("Male.85..csv")+theme(legend.position = "none")+ggtitle("Male 85+ yrs"),
  # pp("All.75.84.csv")+theme(legend.position = "none")+ggtitle("All 75-84 yrs"),
  # pp("Female.75.84.csv")+theme(legend.position = "none")+ggtitle("Female 75-84 yrs"),
  # pp("Male.75.84.csv")+theme(legend.position = "none")+ggtitle("Male 75-84 yrs"),
  # pp("All.65.74.csv")+theme(legend.position = "none")+ggtitle("All 65-74 yrs"),
  # pp("Female.65.74.csv")+theme(legend.position = "none")+ggtitle("Female 65-74 yrs"),
  # pp("Male.65.74.csv")+theme(legend.position = "none")+ggtitle("Male 65-74 yrs"),
  # pp("All.45.64.csv")+theme(legend.position = "none")+ggtitle("All 45-64 yrs"),
  # pp("Female.45.64.csv")+theme(legend.position = "none")+ggtitle("Female 45-64 yrs"),
  # pp("Male.45.64.csv")+theme(legend.position = "none")+ggtitle("Male 45-64 yrs"),
  # bottom = legendgrob, ncol = 3)

  ## March onwards order of plots with pdf dimensions width = 15, height = 5
  pp("Female.85..csv")+theme(legend.position = "none")+ggtitle("Female 85+ yrs"),
  pp("Female.75.84.csv")+theme(legend.position = "none")+ggtitle("Female 75-84 yrs"),
  pp("Female.65.74.csv")+theme(legend.position = "none")+ggtitle("Female 65-74 yrs"),
  pp("Female.45.64.csv")+theme(legend.position = "none")+ggtitle("Female 45-64 yrs"),
  pp("Female.15.44.csv")+theme(legend.position = "none")+ggtitle("Female 15-44 yrs"),
  pp("Male.85..csv")+theme(legend.position = "none")+ggtitle("Male 85+ yrs"),
  pp("Male.75.84.csv")+theme(legend.position = "none")+ggtitle("Male 75-84 yrs"),
  pp("Male.65.74.csv")+theme(legend.position = "none")+ggtitle("Male 65-74 yrs"),
  pp("Male.45.64.csv")+theme(legend.position = "none")+ggtitle("Male 45-64 yrs"),
  pp("Male.15.44.csv")+theme(legend.position = "none")+ggtitle("Male 15-44 yrs"),
  bottom = legendgrob, ncol=5)
dev.off()

# Plot younger age groups all sexes (full forecast only)
pdf("Deaths_YoungerAllsex_fullforecast.pdf", width = 10, height = 7)
grid.arrange(
  pp("All.15.44.csv")+theme(legend.position = "none")+ggtitle("All 15-44 yrs"),
  pp("Female.15.44.csv")+theme(legend.position = "none")+ggtitle("Female 15-44 yrs"),
  pp("Male.15.44.csv")+theme(legend.position = "none")+ggtitle("Male 15-44 yrs"),
  pp("All.01.14.csv")+theme(legend.position = "none")+ggtitle("All 1-14 yrs"),
  pp("Female.01.14.csv")+theme(legend.position = "none")+ggtitle("Female 1-14 yrs"),
  pp("Male.01.14.csv")+theme(legend.position = "none")+ggtitle("Male 1-14 yrs"),
  pp("All.Under.1.year.csv")+theme(legend.position = "none")+ggtitle("All <1 yr"),
  pp("Female.Under.1.year.csv")+theme(legend.position = "none")+ggtitle("Female <1 yr"),
  pp("Male.Under.1.year.csv")+theme(legend.position = "none")+ggtitle("Male <1 yr"),
  bottom = legendgrob, ncol=3)
dev.off()


##### TABLES ###### ---------------------------------------------------------------------------------------------

# Get forecasts of cumulative expected deaths
setwd("./forecasts")
cumulative_files <- list.files(pattern = "\\_cumulative.csv$")
for (i in cumulative_files) {
  t <- read.csv(i, stringsAsFactors = F)
  i <- sub(".csv", "", i)
  t$region <- sub(".csv","",i)
  colnames(t) <- c("X", "Week.ended",	"X2.5", "X50.",	"X97.5", "fit","region")
  assign(i, t)
}
setwd('..')


cumulative_expected <- rbind(Yorkshire.and.The.Humber_cumulative, North.East_cumulative, North.West_cumulative, 
                             East.Midlands_cumulative, West.Midlands_cumulative, East_cumulative, London_cumulative,
                             South.West_cumulative, South.East_cumulative, England_cumulative, Wales_cumulative, 
                             All.85._cumulative, All.75.84_cumulative, All.65.74_cumulative, All.45.64_cumulative,
                             Female.85._cumulative, Female.75.84_cumulative, Female.65.74_cumulative, Female.45.64_cumulative,
                             Male.85._cumulative, Male.75.84_cumulative, Male.65.74_cumulative, Male.45.64_cumulative, #)
                             All.01.14_cumulative,All.15.44_cumulative,All.Under.1.year_cumulative)

cumulative_expected$region <- gsub("_cumulative", "", cumulative_expected$region)
cumulative_expected$X <- NULL
cumulative_expected$X50. <- NULL
cumulative_expected <- cumulative_expected[cumulative_expected$Week.ended <= max(cleandat$Week.ended, na.rm = TRUE),]
cumulative_expected$Week.ended <- as.Date(cumulative_expected$Week.ended)

# Get cumulative registered all-cause deaths
observedcumulativedeaths <- cleandat[cleandat$Week.ended>=ymd("2020-03-05"),]
observedcumulativedeaths <- observedcumulativedeaths[!is.na(observedcumulativedeaths),]
observedcumulativedeaths <- observedcumulativedeaths %>% mutate_if(is.numeric, cumsum)
observedcumulativedeaths$Week.number <- NULL
observedcumulativedeaths <- reshape2::melt(observedcumulativedeaths, id = "Week.ended")
colnames(observedcumulativedeaths) <- c("Week.ended", "region", "deaths")

# Get cumulative registered COVID-19 deaths
cumulativecovid <- read.csv("./Covid_Registration.csv") 
cumulativecovid$Week.ended<-as.Date(cumulativecovid$Week.ended, origin = "1899-12-30")
cumulativecovid[cumulativecovid == 0] <- NA
cumulativecovid <- cumulativecovid %>% mutate(England = rowSums(dplyr::select(.,North.East,North.West,Yorkshire.and.The.Humber,East.Midlands,West.Midlands,
                                                 East, London, South.East, South.West), na.rm = TRUE))
cumulativecovid[is.na(cumulativecovid)] <- 0
cumulativecovid <- cumulativecovid %>% mutate_if(is.numeric, cumsum)
cumulativecovid$Week.number <- NULL
cumulativecovid <- cumulativecovid[cumulativecovid$Week.ended >= ymd("2020-03-05"),]
cumulativecovid <- reshape2::melt(cumulativecovid, id = "Week.ended")
colnames(cumulativecovid) <- c("Week.ended", "region", "covid")

# Join all cumulative datasets
cumulative <- full_join(observedcumulativedeaths, cumulativecovid)
cumulative <- full_join(cumulative, cumulative_expected)
cumulative <- cumulative %>% drop_na()
colnames(cumulative) <- c("Week.ended", "region","deaths","covid","lwr","upr","fit")
# cumulative non-covid excess
#cumulative <- cumulative %>% mutate(excess.ncovid = deaths - covid - fit) %>%
# mutate(excess.ncovid_lwr = deaths - covid - upr) %>% mutate(excess.ncovid_upr = deaths - covid - lwr)
# cumulative excess
cumulative <- cumulative %>% mutate(excess.deaths = deaths - fit) %>% 
   mutate(excess.deaths_lwr = deaths - upr) %>% mutate(excess.deaths_upr = deaths - lwr)
cumulative$region <- gsub("[.]"," ",cumulative$region)
cumulative$region <- gsub("All", "All:", cumulative$region)
cumulative$region <- gsub("Female", "Female:", cumulative$region)
cumulative$region <- gsub("Male", "Male:", cumulative$region)
cumulative$region <- gsub("45 64", "45-64", cumulative$region)
cumulative$region <- gsub("65 74", "65-74", cumulative$region)
cumulative$region <- gsub("75 84", "75-84", cumulative$region)
cumulative$region <- gsub("85 ", "85+", cumulative$region)


Regional_cumulative <- cumulative %>% filter(region %in% c("Yorkshire and The Humber", "North East", "North West", 
                        "East Midlands", "West Midlands", "East", "London",
                        "South West", "South East", "England", "Wales"))

Agegrp_cumulative <- cumulative %>% filter(region %in% c("All: 85+", "All: 75-84", "All: 65-74", "All: 45-64", 
                                                         "Female: 85+", "Female: 75-84", "Female: 65-74", "Female: 45-64", 
                                                         "Male: 85+", "Male: 75-84", "Male: 65-74", "Male: 45-64"))


# Table 1: Latest cumulative deaths (registered all-cause, expected, COVID-19, excess non-COVID-19) by region
latest_cumulative <- Regional_cumulative %>% filter(Week.ended == max(Week.ended)) %>% 
  mutate(per.expted=100*fit/deaths, 
         per.expted.upr=100*(upr/deaths),
         per.expted.lwr=100*(lwr/deaths),
         per.covid=100*(covid/deaths),
         per.exces.ncovid=100*(deaths-covid-fit)/(deaths),
         per.exces.ncovid.lwr=100*(deaths-covid-upr)/(deaths),
         per.exces.ncovid.upr=100*(deaths-covid-lwr)/(deaths))
latest_cumulative$deaths <- format(latest_cumulative$deaths, big.mark=",", trim = TRUE)
latest_cumulative$exptd <- paste(format(round(latest_cumulative$fit), big.mark=",", trim = TRUE), " (", 
                     format(round(latest_cumulative$per.expted, digits = 1), big.mark=",", trim = TRUE), "%)", 
                     sep = "")
latest_cumulative$exptd_bounds <- paste(format(round(latest_cumulative$lwr), big.mark=",", trim = TRUE), " - ", 
                            format(round(latest_cumulative$upr), big.mark=",", trim = TRUE), " (", 
                            round(latest_cumulative$per.expted.lwr, digits = 1),"% - ", 
                            round(latest_cumulative$per.expted.upr, digits = 1), "%)", sep = "")

latest_cumulative$excess_ncovid <- paste(format(round(latest_cumulative$excess.ncovid), big.mark=",", trim = TRUE), " (", 
                            round(latest_cumulative$per.exces.ncovid, digits = 1), "%)", sep = "")
latest_cumulative$exces_bounds <- paste(format(round(latest_cumulative$excess.ncovid_lwr), big.mark=",", trim = TRUE), " - ", 
                            format(round(latest_cumulative$excess.ncovid_upr), big.mark=",", trim = TRUE), " (", 
                            round(latest_cumulative$per.exces.ncovid.lwr, digits = 1),"% - ", 
                            round(latest_cumulative$per.exces.ncovid.upr, digits = 1), "%)", sep = "")

latest_cumulative$covid <- paste(format(round(latest_cumulative$covid), big.mark=",", trim = TRUE), " (", 
                         round(latest_cumulative$per.covid, digits = 1), "%)", sep = "")
latestcumulative_regional <- latest_cumulative %>% select('region', 'deaths', 'exptd', 'exptd_bounds', 'covid', 'excess_ncovid', 'exces_bounds')
colnames(latestcumulative_regional) <- c("Geographic unit", "Reported all-cause deaths", "Expected (%)", "Expected 95% confidence interval (%)", "COVID-19 (%)",
                            "Excess non-COVID-19 (%)", "Excess non-COVID-19 95% confidence interval (%)")
latestcumulative_regional <- latestcumulative_regional[match(c("England", "North West", "North East", "Yorkshire and The Humber", "West Midlands", "East Midlands", "East", 
                                  "South West", "London", "South East", "Wales"),latestcumulative_regional$`Geographic unit`),]
write.csv(latestcumulative_regional, "Table1_cumulativeregional.csv")


# Table 2: Latest cumulative deaths (registered all-cause, expected, COVID-19, excess non-COVID-19) by age group and sex
latest_cumulative_agegrp <- Agegrp_cumulative %>% filter(Week.ended == max(Week.ended)) %>% 
  mutate(per.expted=100*fit/deaths, 
         per.expted.upr=100*(upr/deaths),
         per.expted.lwr=100*(lwr/deaths),
         per.covid=100*(covid/deaths),
         per.exces.ncovid=100*(deaths-covid-fit)/(deaths),
         per.exces.ncovid.lwr=100*(deaths-covid-upr)/(deaths),
         per.exces.ncovid.upr=100*(deaths-covid-lwr)/(deaths))
latest_cumulative_agegrp$deaths <- format(latest_cumulative_agegrp$deaths, big.mark=",", trim = TRUE)
latest_cumulative_agegrp$exptd <- paste(format(round(latest_cumulative_agegrp$fit), big.mark=",", trim = TRUE), " (", 
                                 format(round(latest_cumulative_agegrp$per.expted, digits = 1), big.mark=",", trim = TRUE), "%)", 
                                 sep = "")
latest_cumulative_agegrp$exptd_bounds <- paste(format(round(latest_cumulative_agegrp$lwr), big.mark=",", trim = TRUE), " - ", 
                                        format(round(latest_cumulative_agegrp$upr), big.mark=",", trim = TRUE), " (", 
                                        round(latest_cumulative_agegrp$per.expted.lwr, digits = 1),"% - ", 
                                        round(latest_cumulative_agegrp$per.expted.upr, digits = 1), "%)", sep = "")

latest_cumulative_agegrp$excess_ncovid <- paste(format(round(latest_cumulative_agegrp$excess.ncovid), big.mark=",", trim = TRUE), " (", 
                                         round(latest_cumulative_agegrp$per.exces.ncovid, digits = 1), "%)", sep = "")
latest_cumulative_agegrp$exces_bounds <- paste(format(round(latest_cumulative_agegrp$excess.ncovid_lwr), big.mark=",", trim = TRUE), " - ", 
                                        format(round(latest_cumulative_agegrp$excess.ncovid_upr), big.mark=",", trim = TRUE), " (", 
                                        round(latest_cumulative_agegrp$per.exces.ncovid.lwr, digits = 1),"% - ", 
                                        round(latest_cumulative_agegrp$per.exces.ncovid.upr, digits = 1), "%)", sep = "")

latest_cumulative_agegrp$covid <- paste(format(round(latest_cumulative_agegrp$covid), big.mark=",", trim = TRUE), " (", 
                                 round(latest_cumulative_agegrp$per.covid, digits = 1), "%)", sep = "")
latestcumulative_agegrp <- latest_cumulative_agegrp %>% select('region', 'deaths', 'exptd', 'exptd_bounds', 'covid', 'excess_ncovid', 'exces_bounds')
colnames(latestcumulative_agegrp) <- c("Age group", "Reported all-cause deaths", "Expected (%)", "Expected 95% confidence interval (%)", "COVID-19 (%)",
                                         "Excess non-COVID-19 (%)", "Excess non-COVID-19 95% confidence interval (%)")
write.csv(latestcumulative_agegrp, "Table2_cumulativeagegrp.csv")


# Table A2: Weekly deaths (registered all-cause, expected, covid, excess non-covid) in England
Regional_temp <- rbind(ExtractData("Yorkshire.and.The.Humber.csv"),
                       ExtractData("North.East.csv"),
                       ExtractData("North.West.csv"),
                       ExtractData("East.Midlands.csv"),
                       ExtractData("West.Midlands.csv"),
                       ExtractData("East.csv"),
                       ExtractData("London.csv"),
                       ExtractData("South.West.csv"),
                       ExtractData("South.East.csv"),
                       ExtractData("England.csv"),
                       ExtractData("Wales.csv"))
Regional_temp$n<-rowSums(Regional_temp[,c('fit','signal')],na.rm = T)
Regional_temp<-Regional_temp[Regional_temp$Week.ended>=ymd("2020-03-05"),]
Regional_temp$signal<-NULL
Regional_temp$region <- gsub("[.]"," ",Regional_temp$region)
Regional_temp<-Regional_temp %>% mutate(excess.deaths.upr=deaths-covid-lwr,
                                        excess.deaths.lwr=deaths-covid-upr,
                                        excess.deaths.expected=deaths-covid-fit) # this temp used for coefficient/multiplier (see below)

weeklyEngfull<-Regional_temp %>% filter(region == "England") %>% mutate(alldeaths = deaths, 
                                                        exptd = fit,
                                                        per.expted=100*(fit)/(deaths), 
                                                        exptd.upr = (upr),
                                                        per.expted.upr=100*(upr)/(deaths),
                                                        exptd.lwr = (lwr),
                                                        per.expted.lwr=100*(lwr)/(deaths),
                                                        covid.num = (covid),
                                                        per.covid=100*(covid)/(deaths),
                                                        exces.ncovid = (deaths-covid-fit),
                                                        per.exces.ncovid=100*(deaths-covid-fit)/(deaths),
                                                        exces.ncovid.lwr = (deaths-covid-upr),
                                                        per.exces.ncovid.lwr=100*(deaths-covid-upr)/(deaths),
                                                        exces.ncovid.upr = (deaths-covid-lwr),
                                                        per.exces.ncovid.upr=100*(deaths-covid-lwr)/(deaths)) %>%
  select(Week.ended, alldeaths, exptd, per.expted, exptd.upr,per.expted.upr,exptd.lwr,per.expted.lwr,covid.num,
          per.covid,exces.ncovid,per.exces.ncovid,exces.ncovid.lwr,per.exces.ncovid.lwr,exces.ncovid.upr,per.exces.ncovid.upr)
weeklyEngfull$alldeaths <- format(weeklyEngfull$alldeaths, big.mark=",", trim = TRUE)
weeklyEngfull$exptd <- paste(format(round(weeklyEngfull$exptd), big.mark=",", trim = TRUE), " (", 
                     format(round(weeklyEngfull$per.expted, digits = 1), big.mark=",", trim = TRUE), "%)", 
                     sep = "")
weeklyEngfull$exptd_bounds <- paste(format(round(weeklyEngfull$exptd.lwr), big.mark=",", trim = TRUE), " - ", 
                            format(round(weeklyEngfull$exptd.upr), big.mark=",", trim = TRUE), " (", 
                            round(weeklyEngfull$per.expted.lwr, digits = 1),"% - ", 
                            round(weeklyEngfull$per.expted.upr, digits = 1), "%)", sep = "")

weeklyEngfull$exces.ncovid <- paste(format(round(weeklyEngfull$exces.ncovid), big.mark=",", trim = TRUE), " (", 
                            round(weeklyEngfull$per.exces.ncovid, digits = 1), "%)", sep = "")
weeklyEngfull$exces_bounds <- paste(format(round(weeklyEngfull$exces.ncovid.lwr), big.mark=",", trim = TRUE), " - ", 
                            format(round(weeklyEngfull$exces.ncovid.upr), big.mark=",", trim = TRUE), " (", 
                            round(weeklyEngfull$per.exces.ncovid.lwr, digits = 1),"% - ", 
                            round(weeklyEngfull$per.exces.ncovid.upr, digits = 1), "%)", sep = "")

weeklyEngfull$covid.num <- paste(format(round(weeklyEngfull$covid.num), big.mark=",", trim = TRUE), " (", 
                         round(weeklyEngfull$per.covid, digits = 1), "%)", sep = "")
England_tab <- weeklyEngfull %>% select('Week.ended', 'alldeaths', 'exptd', 'exptd_bounds', 'covid.num', 'exces.ncovid', 'exces_bounds')
colnames(England_tab) <- c("Week ended", "Reported all-cause deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
                            "Excess non-COVID", "Excess non-COVID 95% prediction interval (%)")
write.csv(England_tab, "TableA2_weeklyEngland.csv")


# Weekly cumulative excess non-covid by region
# tab3b <- Regional_cumulative %>% group_by(region) %>% 
#   mutate(cumulative.excessncovid = paste(format(as.integer(excess.ncovid), big.mark =",", trim = TRUE), " (",
#                                          format(as.integer(excess.ncovid_lwr), big.mark =",", trim = TRUE), ", ",
#                                          format(as.integer(excess.ncovid_upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
#   select("Week.ended", "region", "cumulative.excessncovid")
# EW_cml <- tab3b[tab3b$region %in% c("England", "Wales"),]
# tab3b <- tab3b[!tab3b$region %in% c("England", "Wales"),]
# tab3b <- rbind(tab3b, EW_cml)
# tab3b <- reshape2::dcast(tab3b, Week.ended ~ region)
# colnames(tab3b)[1] <- "Week ended"
# tab3b <- tab3b[,c(1:3, 5:9, 11:12, 4,10)] #reorder so EW are at the end
# write.csv(tab3b, "TablecumulativeXS_regional.csv")

# Table A3: weekly excess non-COVID-19 deaths by region
tab3bb <- Regional_temp %>% group_by(region) %>% 
  mutate(excess_ncovid = paste(format(as.integer(excess.deaths.expected), big.mark =",", trim = TRUE), " (",
                               format(as.integer(excess.deaths.lwr), big.mark =",", trim = TRUE), " - ",
                               format(as.integer(excess.deaths.upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
  select(Week.ended, region, excess_ncovid)
tab3bb <- reshape2::dcast(tab3bb, Week.ended ~ region)
tab3bb <- tab3bb[,c(1:3, 5:9, 11:12, 4, 10)]
write.csv(tab3bb, "TableA3XS_regional.csv")

# Weekly cumulative excess non-covid by age group and sex
# tab3c <- Agegrp_cumulative %>% group_by(region) %>% 
#   mutate(cumulative.excessncovid = paste(format(round(fit), big.mark =",", trim = TRUE), " (",
#                                          format(round(lwr), big.mark =",", trim = TRUE), ", ",
#                                          format(round(upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
#   select(Week.ended, region, cumulative.excessncovid)
# tab3c <- reshape2::dcast(tab3c, Week.ended ~ region)
# colnames(tab3c)[1] <- "Week ended"
# write.csv(tab3c, "TablecumulativeweeklyXS_agegrp.csv")



# Table A5: Weekly cumulative excess non-COVID-19 deaths by age group and sex
Agegrp_temp<-rbind(ExtractData("All.85..csv"),
                   ExtractData("Male.85..csv"),
                   ExtractData("Female.85..csv"),
                   ExtractData("All.75.84.csv"),
                   ExtractData("Male.75.84.csv"),
                   ExtractData("Female.75.84.csv"),
                   ExtractData("All.65.74.csv"),
                   ExtractData("Male.65.74.csv"),
                   ExtractData("Female.65.74.csv"),
                   ExtractData("All.45.64.csv"),
                   ExtractData("Male.45.64.csv") ,
                   ExtractData("Female.45.64.csv"))
Agegrp_temp$n<-rowSums(Agegrp_temp[,c('fit','signal')],na.rm = T)
Agegrp_temp<-Agegrp_temp[Agegrp_temp$Week.ended>=ymd("2020-03-01"),]
Agegrp_temp$signal<-NULL
Agegrp_temp$region <- gsub("[.]"," ",Agegrp_temp$region)
Agegrp_temp<-Agegrp_temp %>% mutate(excess.deaths.upr=deaths-covid-lwr,
                                    excess.deaths.lwr=deaths-covid-upr,
                                    excess.deaths.expected=deaths-covid-fit) # this temp used for coefficient/multiplier (see below)
Agegrp_temp$region <- gsub("All", "All:", Agegrp_temp$region)
Agegrp_temp$region <- gsub("Female", "Female:", Agegrp_temp$region)
Agegrp_temp$region <- gsub("Male", "Male:", Agegrp_temp$region)
Agegrp_temp$region <- gsub("45 64", "45-64", Agegrp_temp$region)
Agegrp_temp$region <- gsub("65 74", "65-74", Agegrp_temp$region)
Agegrp_temp$region <- gsub("75 84", "75-84", Agegrp_temp$region)
Agegrp_temp$region <- gsub("85 ", "85+", Agegrp_temp$region)
tab3cc <- Agegrp_temp %>% group_by(region) %>% 
  mutate(excess_ncovid = paste(format(as.integer(excess.deaths.expected), big.mark =",", trim = TRUE), " (",
                               format(as.integer(excess.deaths.lwr), big.mark =",", trim = TRUE), " - ",
                               format(as.integer(excess.deaths.upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
  select(Week.ended, region, excess_ncovid)
tab3cc <- reshape2::dcast(tab3cc, Week.ended ~ region)
write.csv(tab3cc, "TableA4weeklyXS_agegrp.csv")

# Table A6: Weekly cumulative non-covid excess deaths by age group and sex
Agegrp_temp<-rbind(ExtractData("All.85..csv"),
                   ExtractData("Male.85..csv"),
                   ExtractData("Female.85..csv"),
                   ExtractData("All.75.84.csv"),
                   ExtractData("Male.75.84.csv"),
                   ExtractData("Female.75.84.csv"),
                   ExtractData("All.65.74.csv"),
                   ExtractData("Male.65.74.csv"),
                   ExtractData("Female.65.74.csv"),
                   ExtractData("All.45.64.csv"),
                   ExtractData("Male.45.64.csv") ,
                   ExtractData("Female.45.64.csv"))
Agegrp_temp$n<-rowSums(Agegrp_temp[,c('fit','signal')],na.rm = T)
Agegrp_temp<-Agegrp_temp[Agegrp_temp$Week.ended>=ymd("2020-03-01"),]
Agegrp_temp$signal<-NULL
Agegrp_temp$region <- gsub("[.]"," ",Agegrp_temp$region)
Agegrp_temp<-Agegrp_temp %>% mutate(excess.deaths.upr=deaths-covid-lwr,
                                    excess.deaths.lwr=deaths-covid-upr,
                                    excess.deaths.expected=deaths-covid-fit) # this temp used for coefficient/multiplier (see below)
Agegrp_temp$region <- gsub("All", "All:", Agegrp_temp$region)
Agegrp_temp$region <- gsub("Female", "Female:", Agegrp_temp$region)
Agegrp_temp$region <- gsub("Male", "Male:", Agegrp_temp$region)
Agegrp_temp$region <- gsub("45 64", "45-64", Agegrp_temp$region)
Agegrp_temp$region <- gsub("65 74", "65-74", Agegrp_temp$region)
Agegrp_temp$region <- gsub("75 84", "75-84", Agegrp_temp$region)
Agegrp_temp$region <- gsub("85 ", "85+", Agegrp_temp$region)
tab3cc <- Agegrp_temp %>% group_by(region) %>% 
  mutate(excess_ncovid = paste(format(as.integer(excess.deaths.expected), big.mark =",", trim = TRUE), " (",
                               format(as.integer(excess.deaths.lwr), big.mark =",", trim = TRUE), " - ",
                               format(as.integer(excess.deaths.upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
  select(Week.ended, region, excess_ncovid)
tab3cc <- reshape2::dcast(tab3cc, Week.ended ~ region)
write.csv(tab3cc, "TableA4weeklyXS_agegrp.csv")

# Table A5: Coefficient multiplier COVID-19 to non-COVID-19 excess deaths
Regional_temp$coeff <- Regional_temp$excess.deaths.expected/Regional_temp$covid
Regional_temp$coeff_lwr <- Regional_temp$excess.deaths.lwr/Regional_temp$covid
Regional_temp$coeff_upr <- Regional_temp$excess.deaths.upr/Regional_temp$covid
Regional_temp <- Regional_temp[Regional_temp$Week.ended>=as.Date("2020-04-03"),]
temp_coeff <- Regional_temp %>% mutate(coeff_bounds = paste(round(Regional_temp$coeff, digits = 2), " (",
                                                            round(Regional_temp$coeff_lwr, digits = 2), " - ",
                                                            round(Regional_temp$coeff_upr, digits = 2), ")", sep =""))
temp_coeff <- temp_coeff %>% select("Week.ended", "region", "coeff_bounds")
temp_coeff$region <- gsub("[.]"," ",temp_coeff$region)
temp_coeff <- reshape2::dcast(temp_coeff, Week.ended~region)
colnames(temp_coeff)[1] <- "Week ended"
write.csv(temp_coeff, "TableA5Coeff_Regional.csv")

### weekly cumulative excess deaths (covid + non-covid)
plot_weeklycumdeaths <- function(cat, tempdf) {
  ggplot(tempdf %>% filter(region == cat)) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(x = Week.ended, y = excess.deaths)) +
    geom_errorbar(aes(x = Week.ended, ymin = excess.deaths_lwr, ymax = excess.deaths_upr)) +
    labs(x = "Week ended", y = "Cumulative excess deaths", title = cat) +
    theme_minimal() + theme_bw()
}

regional_weeklycumXS <- lapply(unique(Regional_cumulative$region), 
                               function(x) plot_weeklycumdeaths(cat = x, tempdf = Regional_cumulative))
ggarrange(regional_weeklycumXS[[1]],regional_weeklycumXS[[2]],regional_weeklycumXS[[3]], regional_weeklycumXS[[4]],
          regional_weeklycumXS[[5]],regional_weeklycumXS[[6]],regional_weeklycumXS[[7]],regional_weeklycumXS[[8]],
          regional_weeklycumXS[[9]]) #pdf 10x6.5
ggarrange(regional_weeklycumXS[[11]], regional_weeklycumXS[[10]]) # pdf 7x3.5

agegrp_weeklycumXS <- lapply(unique(Agegrp_cumulative$region), 
                             function(x) plot_weeklycumdeaths(cat = x, tempdf = Agegrp_cumulative))
ggarrange(agegrp_weeklycumXS[[12]],agegrp_weeklycumXS[[11]],agegrp_weeklycumXS[[10]],agegrp_weeklycumXS[[9]],
          agegrp_weeklycumXS[[8]],agegrp_weeklycumXS[[7]],agegrp_weeklycumXS[[6]],agegrp_weeklycumXS[[5]], 
          nrow = 2, ncol = 4) #pdf 15x6
ggarrange(agegrp_weeklycumXS[[4]],agegrp_weeklycumXS[[3]],agegrp_weeklycumXS[[2]],agegrp_weeklycumXS[[1]]) #pdf 6x5

