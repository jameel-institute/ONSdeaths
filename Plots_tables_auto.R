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
#source("CleanUp.R")
#source("functions.R")
#file<-"/Volumes/GoogleDrive/My Drive/Covid-19/Mortality/CleanedData.csv"
setwd("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths")

#clean cleaned data
file <- "/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/CleanedData.csv"
cleancleaneddata <- function() {
  data<-read.csv(file,stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended,"%Y-%m-%d")
  data$last.week.year<-0
  data$year<-format(data$Week.ended,"%Y")
  data[(data$Week.number==52 & data$year %in% c(2010,2011,2012,2013,2014,2016,2017,2018,2019)),'last.week.year']<-1
  data[data$Week.number==53,'last.week.year']<-1
  pos<-which(data$last.week.year ==1)
  
  for (i in pos){
  x1<-data[data$X==i, 4:42]
  x2<-data[data$X==i+1, 4:42]
  x<-(x1+x2)/2
  data[data$X==i, 4:42]<-x
  data[data$X==i+1, 4:42]<-x
}
  end1<-ymd("2020-03-01")
  n<-length(data$Week.ended[data$Week.ended>=end1])
  data$respiratory<-rowSums(data[,c('ICD.10.v.2010',
                                  'All.respiratory.diseases..ICD.10.J00.J99...ICD.10.v.2010',
                                  'All.respiratory.diseases..ICD.10.J00.J99..ICD.10.v.2013..IRIS.',
                                  'ICD.10.v.2010..NCHS.')], na.rm=T)
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
               "England",
               "respiratory")]
  return(data)
}
cleandat <- cleancleaneddata()

# extract ggplot legend
g_legend <- function(a.gplot){ # function for legend grob
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# extract data
ExtractData<-function(i){
  data<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",i),stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  data <- data[data$Week.ended<=as.Date(max(cleandat$Week.ended)),] # until most recent date updated
  data[,sub(".csv","",i)] <- cleandat[,sub(".csv", "",i)]
  
  data<-data[,c('Week.ended','fit','lwr','upr','signal',sub(".csv", "",i))]
  colnames(data)<-c('Week.ended','fit','lwr','upr','signal','deaths')
  
  temp1<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv")
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

pp<-function(i){ # function that plots total observed, predicted, forecasted, and excess deaths
  data<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",i),
                 stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  data<-data[,c('Week.ended','fit','lwr','upr','signal',sub(".csv", "",i))]
  colnames(data)<-c('Week.ended','fit','lwr','upr','signal','var') #signal = predicted ; var = observed ; fit = forecasted
  data <- data[data$Week.ended<=as.Date(max(cleandat$Week.ended)),] # until most recent date updated
  data$var <- cleandat[,sub(".csv", "",i)]
  
  temp1<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv")
  temp1[temp1 == 0] <- NA
  temp1$Week.ended<-as.Date(temp1$Week.ended, origin = "1899-12-30")
  
  if (i == "England.csv") {
    temp1 <- temp1 %>%
      mutate(England = rowSums(dplyr::select(., North.East,North.West,Yorkshire.and.The.Humber,East.Midlands,West.Midlands,
                                East, London, South.East, South.West), na.rm = TRUE))
  }
  
  temp1<-temp1[,c('Week.ended',sub(".csv", "",i))]
  colnames(temp1)<-c('Week.ended','covid')
  #data <- data[data$Week.ended>=as.Date("2020-03-01"),] # for March onwards
  data <- data[data$Week.ended>=as.Date("2019-07-01"),] # for full forecast
  data<-merge(data,temp1[,],by.x='Week.ended',by.y='Week.ended',all.x=TRUE)
  data$var_covid <- data$var - data$covid
  data$covid <- NULL
  mdf <- reshape2::melt(data, id.var = c("Week.ended", "lwr", "upr"))
  #mdf <- mdf[!(mdf$variable == "signal"),] # for March onwards
  mdf$variable <- factor(mdf$variable, levels=c("fit","var","var_covid", "signal"), # # take out signal for March onwards
                         labels=c(paste("Expected 06/03-", format(max(mdf$Week.ended), "%d/%m"), sep = ""),
                                  "Registered all-cause","Registered non-COVID deaths", "Predicted")) #  # take out Predicted for March onwards
  p<-ggplot(mdf, aes(x = Week.ended, y = value, colour = variable))+
    geom_ribbon(aes(ymin = lwr, ymax = upr), colour = NA, fill = "grey70",alpha=0.5)+
    geom_line() + 
    #scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%d/%m") + # for March onwards
    scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%m/%y", date_breaks = "1 month") + # for full forecast
    theme_bw(base_size = 12, base_family = "Helvetica")+ theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(gsub("[.]"," ",sub(".csv", "",i)))+
    scale_colour_manual(values = c("Black","Red", "Purple", "Blue")) + # # take out Blue for March onwards
    labs(colour = "Deaths", y ="Deaths", x = "")
  
  return(p)
}

legendgrob <- g_legend(pp("Yorkshire.and.The.Humber.csv") + theme(legend.position = "bottom"))

# plot of England & Wales from March onwards
pdf("Deaths_EW_MarOnwards.pdf", width = 10, height = 4) #_fullforecast
grid.arrange(pp("England.csv") + theme(legend.position = "none"), pp("Wales.csv") +theme(legend.position = "none"),
             bottom = legendgrob, ncol=2) #pdf 7x3
dev.off()

# plot England regions
pdf("Regional_forecasts_England_fullforecast.pdf", width = 10, height = 7) #_MarOnwards
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
  bottom = legendgrob, ncol=3) #pdf 10 x 7
dev.off()

# plot older age groups (all sexes)
pdf("Deaths_Allolder_MarOnwards.pdf", width = 7, height = 5) #_fullforecast
grid.arrange(pp("All.45.64.csv")+theme(legend.position = "none")+ggtitle("All 45-64 yrs"),#+ylim(0, 10000),
             pp("All.65.74.csv")+theme(legend.position = "none")+ggtitle("All 65-74 yrs"),#+ylim(0, 10000),
             pp("All.75.84.csv")+theme(legend.position = "none")+ggtitle("All 75-84 yrs"),#+ylim(0, 10000),
             pp("All.85..csv")+theme(legend.position = "none")+ggtitle("All 85+ yrs"),#+ylim(0, 10000),
             bottom = legendgrob, ncol=2) #pdf 7x5
dev.off()

# plot all sex and older ages
pdf("Deaths_AllolderAllsex_fullforecast.pdf", width = 10, height = 7) #_MarOnwards
grid.arrange(
  # # full forecast order width = 10, height = 7
  pp("All.85..csv")+theme(legend.position = "none")+ggtitle("All 85+ yrs"),
  pp("Female.85..csv")+theme(legend.position = "none")+ggtitle("Female 85+ yrs"),
  pp("Male.85..csv")+theme(legend.position = "none")+ggtitle("Male 85+ yrs"),
  pp("All.75.84.csv")+theme(legend.position = "none")+ggtitle("All 75-84 yrs"),
  pp("Female.75.84.csv")+theme(legend.position = "none")+ggtitle("Female 75-84 yrs"),
  pp("Male.75.84.csv")+theme(legend.position = "none")+ggtitle("Male 75-84 yrs"),
  pp("All.65.74.csv")+theme(legend.position = "none")+ggtitle("All 65-74 yrs"),
  pp("Female.65.74.csv")+theme(legend.position = "none")+ggtitle("Female 65-74 yrs"),
  pp("Male.65.74.csv")+theme(legend.position = "none")+ggtitle("Male 65-74 yrs"),
  pp("All.45.64.csv")+theme(legend.position = "none")+ggtitle("All 45-64 yrs"),
  pp("Female.45.64.csv")+theme(legend.position = "none")+ggtitle("Female 45-64 yrs"),
  pp("Male.45.64.csv")+theme(legend.position = "none")+ggtitle("Male 45-64 yrs"),
  bottom = legendgrob, ncol = 3)
  # March onwards order, width = 13, height = 5
  # pp("Female.85..csv")+theme(legend.position = "none")+ggtitle("Female 85+ yrs"),
  # pp("Female.75.84.csv")+theme(legend.position = "none")+ggtitle("Female 75-84 yrs"),
  # pp("Female.65.74.csv")+theme(legend.position = "none")+ggtitle("Female 65-74 yrs"),
  # pp("Female.45.64.csv")+theme(legend.position = "none")+ggtitle("Female 45-64 yrs"),
  # pp("Female.15.44.csv")+theme(legend.position = "none")+ggtitle("Female 15-44 yrs"),
  # pp("Male.85..csv")+theme(legend.position = "none")+ggtitle("Male 85+ yrs"),
  # pp("Male.75.84.csv")+theme(legend.position = "none")+ggtitle("Male 75-84 yrs"),
  # pp("Male.65.74.csv")+theme(legend.position = "none")+ggtitle("Male 65-74 yrs"),
  # pp("Male.45.64.csv")+theme(legend.position = "none")+ggtitle("Male 45-64 yrs"),
  # pp("Male.15.44.csv")+theme(legend.position = "none")+ggtitle("Male 15-44 yrs"),
  # bottom = legendgrob, ncol=5) #pdf 7x10
dev.off()

# plot all sex and older ages - full forecast only 
pdf("Deaths_YoungerAllsex_fullforecast.pdf", width = 10, height = 7) #_MarOnwards
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
  bottom = legendgrob, ncol=3) #pdf 7x10
dev.off()


# TABLES

setwd("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts")
cumulative_files <- list.files(pattern = "\\_cumulative.csv$")
for (i in cumulative_files) {
  t <- read.csv(i, stringsAsFactors = F)
  i <- sub(".csv", "", i)
  t$region <- sub(".csv","",i)
  assign(i, t)
}
setwd("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths")
# cumulative expected
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
cumulative_expected <- cumulative_expected[cumulative_expected$Week.ended <= max(cleandat$Week.ended),]
cumulative_expected$Week.ended <- as.Date(cumulative_expected$Week.ended)

# cumulative observed deaths
observedcumulativedeaths <- cleandat[cleandat$Week.ended>=ymd("2020-03-05"),]
observedcumulativedeaths <- observedcumulativedeaths %>% mutate_if(is.numeric, cumsum)
observedcumulativedeaths$Week.number <- NULL
observedcumulativedeaths <- reshape2::melt(observedcumulativedeaths, id = "Week.ended")
colnames(observedcumulativedeaths) <- c("Week.ended", "region", "deaths")

cumulativecovid <- read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv") 
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

cumulative <- full_join(observedcumulativedeaths, cumulativecovid)
cumulative <- full_join(cumulative, cumulative_expected)
cumulative <- cumulative %>% drop_na()
cumulative <- cumulative %>% mutate(excess.ncovid = deaths - covid - fit) %>% 
  mutate(excess.ncovid_lwr = deaths - covid - upr) %>% mutate(excess.ncovid_upr = deaths - covid - lwr)
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


# Table 1: Latest cumulative deaths (all, expected, covid, excess non-covid) by region
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
EW <- latest_cumulative[latest_cumulative$region %in% c("England", "Wales"),]
latest_cumulative <- latest_cumulative[!latest_cumulative$region %in% c("England", "Wales"),]
latest_cumulative <- rbind(latest_cumulative, EW)
latestcumulative_regional <- latest_cumulative %>% select('region', 'deaths', 'exptd', 'exptd_bounds', 'covid', 'excess_ncovid', 'exces_bounds')
colnames(latestcumulative_regional) <- c("Region", "Reported all-cause deaths", "Expected (%)", "Expected 95% confidence interval (%)", "COVID-19 (%)",
                            "Excess non-COVID-19 (%)", "Excess non-COVID-19 95% confidence interval (%)")
latestcumulative_regional <- latestcumulative_regional[match(c("England", "North West", "North East", "Yorkshire and The Humber", "West Midlands", "East Midlands", "East", 
                                  "South West", "London", "South East", "Wales"),latestcumulative_regional$Region),]
write.csv(latestcumulative_regional, "Table1_cumulativeregional.csv")


# Table 2: Latest cumulative deaths (all, expected, covid, excess non-covid) by age group and sex
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


# Table A2: Weekly deaths (all, expected, covid, excess non-covid) in England
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
weeklyEngfull$exptd_bounds <- paste(format(round(weeklyEngfull$exptd.lwr), big.mark=",", trim = TRUE), " -", 
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


# Table A3: Weekly cumulative excess non-covid by region
tab3b <- Regional_cumulative %>% group_by(region) %>% 
  mutate(cumulative.excessncovid = paste(format(as.integer(excess.ncovid), big.mark =",", trim = TRUE), " (",
                                         format(as.integer(excess.ncovid_lwr), big.mark =",", trim = TRUE), ", ",
                                         format(as.integer(excess.ncovid_upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
  select("Week.ended", "region", "cumulative.excessncovid")
EW_cml <- tab3b[tab3b$region %in% c("England", "Wales"),]
tab3b <- tab3b[!tab3b$region %in% c("England", "Wales"),]
tab3b <- rbind(tab3b, EW_cml)
tab3b <- reshape2::dcast(tab3b, Week.ended ~ region)
colnames(tab3b)[1] <- "Week ended"
tab3b <- tab3b[,c(1:3, 5:9, 11:12, 4,10)] #reorder so EW are at the end
write.csv(tab3b, "TableA3cumulativeXS_regional.csv")

tab3bb <- Regional_temp %>% group_by(region) %>% 
  mutate(excess_ncovid = paste(format(as.integer(excess.deaths.expected), big.mark =",", trim = TRUE), " (",
                               format(as.integer(excess.deaths.lwr), big.mark =",", trim = TRUE), " - ",
                               format(as.integer(excess.deaths.upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
  select(Week.ended, region, excess_ncovid)
tab3bb <- reshape2::dcast(tab3bb, Week.ended ~ region)
tab3bb <- tab3bb[,c(1:3, 5:9, 11:12, 4, 10)]
write.csv(tab3bb, "TableA3XS_regional.csv")

# Table A4: Weekly cumulative excess non-covid by age group and sex
tab3c <- Agegrp_cumulative %>% group_by(region) %>% 
  mutate(cumulative.excessncovid = paste(format(round(fit), big.mark =",", trim = TRUE), " (",
                                         format(round(lwr), big.mark =",", trim = TRUE), ", ",
                                         format(round(upr), big.mark =",", trim = TRUE), ")", sep = "")) %>%
  select(Week.ended, region, cumulative.excessncovid)
tab3c <- reshape2::dcast(tab3c, Week.ended ~ region)
colnames(tab3c)[1] <- "Week ended"
write.csv(tab3c, "TableA4cumulativeweeklyXS_agegrp.csv")

# Table A4: Weekly cumulative excess non-covid by age group and sex
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


# Table A5: Coefficient multiplier covid to non-covid excess deaths
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

####### NOT USED ##### ---------------------------------------------------------------------------------------
# # OLD Table A2: EXCESS DEATHS EACH WEEK FOR EACH REGION
# # weekly excess non-covid
# tab3 <- Regional_temp %>% group_by(region) %>% #filter(region %in% c("England", "Wales")) %>% 
#   mutate(excess.ncovid = paste(format(round(excess.deaths.expected), big.mark = ",", trim = TRUE), " (",
#                                format(round(excess.deaths.lwr), big.mark = ",", trim = TRUE), ",",
#                                format(round(excess.deaths.upr), big.mark = ",", trim = TRUE), ")", sep = "")) %>%
#   select(Week.ended, region, excess.ncovid)
# tab3 <- reshape2::dcast(tab3, region~Week.ended)
# tab3$region <- gsub("[.]"," ",tab3$region)
# EW <- tab3[tab3$region %in% c("England", "Wales"),]
# tab3 <- tab3[!tab3$region %in% c("England", "Wales"),]
# tab3 <- rbind(tab3, EW)
# write.csv(tab3, "TableXSncovid_region.csv")

# OLD TABLE 1: REGIONAL
# temp3<-Regional_temp %>% group_by(region) %>% summarise(alldeaths = sum(deaths), 
#                                                         exptd = sum(fit),
#                                                         per.expted=100*sum(fit)/sum(deaths), 
#                                                         exptd.upr = sum(upr),
#                                                         per.expted.upr=100*sum(upr)/sum(deaths),
#                                                         exptd.lwr = sum(lwr),
#                                                         per.expted.lwr=100*sum(lwr)/sum(deaths),
#                                                         covid.num = sum(covid),
#                                                         per.covid=100*sum(covid)/sum(deaths),
#                                                         exces.ncovid = sum(deaths-covid-fit),
#                                                         per.exces.ncovid=100*sum(deaths-covid-fit)/sum(deaths),
#                                                         exces.ncovid.lwr = sum(deaths-covid-upr),
#                                                         per.exces.ncovid.lwr=100*sum(deaths-covid-upr)/sum(deaths),
#                                                         exces.ncovid.upr = sum(deaths-covid-lwr),
#                                                         per.exces.ncovid.upr=100*sum(deaths-covid-lwr)/sum(deaths))
# temp3$alldeaths <- format(temp3$alldeaths, big.mark=",", trim = TRUE)
# temp3$exptd <- paste(format(round(temp3$exptd), big.mark=",", trim = TRUE), " (", 
#                      format(round(temp3$per.expted, digits = 1), big.mark=",", trim = TRUE), "%)", 
#                      sep = "")
# temp3$exptd_bounds <- paste(format(round(temp3$exptd.lwr), big.mark=",", trim = TRUE), " -", 
#                             format(round(temp3$exptd.upr), big.mark=",", trim = TRUE), " (", 
#                             round(temp3$per.expted.lwr, digits = 1),"% - ", 
#                             round(temp3$per.expted.upr, digits = 1), "%)", sep = "")
# 
# temp3$exces.ncovid <- paste(format(round(temp3$exces.ncovid), big.mark=",", trim = TRUE), " (", 
#                             round(temp3$per.exces.ncovid, digits = 1), "%)", sep = "")
# temp3$exces_bounds <- paste(format(round(temp3$exces.ncovid.lwr), big.mark=",", trim = TRUE), " - ", 
#                             format(round(temp3$exces.ncovid.upr), big.mark=",", trim = TRUE), " (", 
#                             round(temp3$per.exces.ncovid.lwr, digits = 1),"% - ", 
#                             round(temp3$per.exces.ncovid.upr, digits = 1), "%)", sep = "")
# 
# temp3$covid.num <- paste(format(round(temp3$covid.num), big.mark=",", trim = TRUE), " (", 
#                          round(temp3$per.covid, digits = 1), "%)", sep = "")
# temp3$region <- gsub("[.]"," ",temp3$region)
# EW <- temp3[temp3$region %in% c("England", "Wales"),]
# temp3 <- temp3[!temp3$region %in% c("England", "Wales"),]
# temp3 <- rbind(temp3, EW)
# regional_tab <- temp3 %>% select('region', 'alldeaths', 'exptd', 'exptd_bounds', 'covid.num', 'exces.ncovid', 'exces_bounds')
# colnames(regional_tab) <- c("Region", "Reported all-cause deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
#                             "Excess non-COVID", "Excess non-COVID 95% prediction interval (%)")
# write.csv(regional_tab, "Table1_cumulativeregion.csv")

# # AGE GROUP: OLD TABLE 2

# temp4<-Agegrp_temp %>% group_by(region) %>% summarise(alldeaths = sum(deaths),
#                                                       exptd = sum(fit),
#                                                       per.expted=100*sum(fit)/sum(deaths), 
#                                                       exptd.upr = sum(upr),
#                                                       per.expted.upr=100*sum(upr)/sum(deaths),
#                                                       exptd.lwr = sum(lwr),
#                                                       per.expted.lwr=100*sum(lwr)/sum(deaths),
#                                                       covid.num = sum(covid),
#                                                       per.covid=100*sum(covid)/sum(deaths),
#                                                       exces.ncovid = sum(deaths-covid-fit),
#                                                       per.exces.ncovid=100*sum(deaths-covid-fit)/sum(deaths),
#                                                       exces.ncovid.lwr = sum(deaths-covid-upr),
#                                                       per.exces.ncovid.lwr=100*sum(deaths-covid-upr)/sum(deaths),
#                                                       exces.ncovid.upr = sum(deaths-covid-lwr),
#                                                       per.exces.ncovid.upr=100*sum(deaths-covid-lwr)/sum(deaths))
# temp4$alldeaths <- format(temp4$alldeaths, big.mark=",", trim = TRUE)
# temp4$exptd <- paste(format(round(temp4$exptd), big.mark=",", trim = TRUE), " (", 
#                      format(round(temp4$per.expted, digits = 1), big.mark=",", trim = TRUE), "%)", 
#                      sep = "")
# temp4$exptd_bounds <- paste(format(round(temp4$exptd.lwr), big.mark=",", trim = TRUE), " -", 
#                             format(round(temp4$exptd.upr), big.mark=",", trim = TRUE), " (", 
#                             round(temp4$per.expted.lwr, digits = 1),"% - ", 
#                             round(temp4$per.expted.upr, digits = 1), "%)", sep = "")
# 
# temp4$exces.ncovid <- paste(format(round(temp4$exces.ncovid), big.mark=",", trim = TRUE), " (", 
#                             round(temp4$per.exces.ncovid, digits = 1), "%)", sep = "")
# temp4$exces_bounds <- paste(format(round(temp4$exces.ncovid.lwr), big.mark=",", trim = TRUE), " - ", 
#                             format(round(temp4$exces.ncovid.upr), big.mark=",", trim = TRUE), " (", 
#                             round(temp4$per.exces.ncovid.lwr, digits = 1),"% - ", 
#                             round(temp4$per.exces.ncovid.upr, digits = 1), "%)", sep = "")
# 
# temp4$covid.num <- paste(format(round(temp4$covid.num), big.mark=","), " (", 
#                          round(temp4$per.covid, digits = 1), "%)", sep = "")
# temp4$region <- gsub("[.]"," ",temp4$region)
# temp4$region <- gsub("All", "All:", temp4$region)
# temp4$region <- gsub("Female", "Female:", temp4$region)
# temp4$region <- gsub("Male", "Male:", temp4$region)
# temp4$region <- gsub("45 64", "45-64", temp4$region)
# temp4$region <- gsub("65 74", "65-74", temp4$region)
# temp4$region <- gsub("75 84", "75-84", temp4$region)
# temp4$region <- gsub("85 ", "85+", temp4$region)
# agegrp_tab <- temp4 %>% select('region','alldeaths', 'exptd', 'exptd_bounds', 'covid.num', 'exces.ncovid', 'exces_bounds')
# colnames(agegrp_tab) <- c("Age group", "Reported all-cause deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
#                             "Excess non-COVID", "Excess non-COVID 95% prediction interval (%)")
# write.csv(agegrp_tab, "Table2.csv")
