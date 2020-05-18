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
file <- "/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/CleanedData.csv"

function(a.gplot){ # function for legend grob
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ExtractData<-function(i){
  data<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",i),stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  
  data<-data[,c('Week.ended','fit','lwr','upr','signal',sub(".csv", "",i))]
  
  colnames(data)<-c('Week.ended','fit','lwr','upr','signal','deaths')
  
  temp1<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv")
  if (i == "England.csv") {
    temp1 <- temp1 %>% mutate(England = North.East + North.West + Yorkshire.and.The.Humber + East.Midlands + West.Midlands + 
                                East + London + South.East + South.West)
  }
  temp1$Week.ended<-as.Date(temp1$Week.ended, origin = "1899-12-30")
  temp1[temp1 == 0] <- NA
  temp1<-temp1[,c('Week.ended',sub(".csv", "",i))]
  colnames(temp1)<-c('Week.ended','covid')
  data<-merge(data,temp1[,],by.x='Week.ended',by.y='Week.ended',all.x=TRUE)
  data$region<-sub(".csv", "",i)
  data$covid[is.na(data$covid)] <- 0
  return(data)}

pp<-function(i){ # function that plots total observed, predicted, forecasted, and excess deaths
  data<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",i),
                 stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  data<-data[,c('Week.ended','fit','lwr','upr','signal',sub(".csv", "",i))]
  colnames(data)<-c('Week.ended','fit','lwr','upr','signal','var')
  
  temp1<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv")
  temp1[temp1 == 0] <- NA
  temp1$Week.ended<-as.Date(temp1$Week.ended, origin = "1899-12-30")
  
  if (i == "England.csv") {
    temp1 <- temp1 %>% mutate(England = North.East + North.West + Yorkshire.and.The.Humber + East.Midlands + West.Midlands + 
                                East + London + South.East + South.West)
  }
  
  temp1<-temp1[,c('Week.ended',sub(".csv", "",i))]
  colnames(temp1)<-c('Week.ended','covid')
  
  data<-merge(data,temp1[,],by.x='Week.ended',by.y='Week.ended',all.x=TRUE)
  data$var_covid <- data$var - data$covid
  data$covid <- NULL
  data <- data[data$Week.ended>=as.Date("2020-03-01"),] # for March onwards
  mdf <- reshape2::melt(data, id.var = c("Week.ended", "lwr", "upr"))
  mdf <- mdf[!(mdf$variable == "signal"),] # for March onwards
  mdf$variable <- factor(mdf$variable, levels=c("fit","var","var_covid"), #, "signal"), # comment out signal for March onwards
                         labels=c(paste("Forecasted 06/03-", format(max(mdf$Week.ended), "%d/%m"), sep = ""),
                                  "Observed all-cause","Registered non-COVID deaths"))#, "Predicted")) # take out Predicted for March onwards
  p<-ggplot(mdf, aes(x = Week.ended, y = value, colour = variable))+
    geom_ribbon(aes(ymin = lwr, ymax = upr), colour = NA, fill = "grey70",alpha=0.5)+
    geom_line() + 
    scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%d/%m") + # for March onwards
    #scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%m/%y", date_breaks = "1 month") + # for full forecast
    theme_bw(base_size = 12, base_family = "Helvetica")+ theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(gsub("[.]"," ",sub(".csv", "",i)))+
    scale_colour_manual(values = c("Black","Red", "Purple")) + #, "Blue")) + # take out Blue for March onwards
    labs(colour = "Deaths", y ="Deaths", x = "")
  
  return(p)
}

legendgrob <- g_legend(pp("Yorkshire.and.The.Humber.csv") + theme(legend.position = "bottom"))

# plot of England & Wales from March onwards
grid.arrange(pp("England.csv") + theme(legend.position = "none"), pp("Wales.csv") +theme(legend.position = "none"),
             bottom = legendgrob, ncol=2) #pdf 7x3

# plot England regions
grid.arrange(pp("Yorkshire.and.The.Humber.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("North.East.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("North.West.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("East.Midlands.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("West.Midlands.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("East.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("London.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("South.East.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  pp("South.West.csv")+ylim(0, 3500)+theme(legend.position = "none"),
  bottom = legendgrob, ncol=3) #pdf 10 x 7

# plot older age groups (all sexes)
grid.arrange(pp("All.85..csv")+theme(legend.position = "none")+ggtitle("All 85+ yrs")+ylim(0, 10000),
             pp("All.75.84.csv")+theme(legend.position = "none")+ggtitle("All 75-84 yrs")+ylim(0, 10000),
             pp("All.65.74.csv")+theme(legend.position = "none")+ggtitle("All 65-74 yrs")+ylim(0, 10000),
             pp("All.45.64.csv")+theme(legend.position = "none")+ggtitle("All 45-64 yrs")+ylim(0, 10000),
             bottom = legendgrob, ncol=2) #pdf 7x5

# plot all sex and older ages
grid.arrange(pp("Male.85..csv")+theme(legend.position = "none")+ggtitle("Male 85+ yrs"),
             pp("Female.85..csv")+theme(legend.position = "none")+ggtitle("Female 85+ yrs"),
             pp("Male.75.84.csv")+theme(legend.position = "none")+ggtitle("Male 75-84 yrs"),
             pp("Female.75.84.csv")+theme(legend.position = "none")+ggtitle("Female 75-84 yrs"),
             pp("Male.65.74.csv")+theme(legend.position = "none")+ggtitle("Male 65-74 yrs"),
             pp("Female.65.74.csv")+theme(legend.position = "none")+ggtitle("Female 65-74 yrs"),
             pp("Male.45.64.csv")+theme(legend.position = "none")+ggtitle("Male 45-64 yrs"),
             pp("Female.45.64.csv")+theme(legend.position = "none")+ggtitle("Female 45-64 yrs"),
             pp("Male.15.44.csv")+theme(legend.position = "none")+ggtitle("Male 15-44 yrs"),
             pp("Female.15.44.csv")+theme(legend.position = "none")+ggtitle("Female 15-44 yrs"),
             bottom = legendgrob, ncol=2) #pdf 7x10

# tables
# Regional table
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

Regional_temp<-Regional_temp %>% mutate(excess.deaths.upr=deaths-covid-lwr, 
                                        excess.deaths.lwr=deaths-covid-upr, 
                                        excess.deaths.expected=deaths-covid-fit) # this temp used for coefficient/multiplier (see below)
temp3<-Regional_temp %>% group_by(region) %>% summarise(alldeaths = sum(deaths), 
                                                        exptd = sum(fit),
                                                        per.expted=100*sum(fit)/sum(deaths), 
                                                        exptd.upr = sum(upr),
                                                        per.expted.upr=100*sum(upr)/sum(deaths),
                                                        exptd.lwr = sum(lwr),
                                                        per.expted.lwr=100*sum(lwr)/sum(deaths),
                                                        covid.num = sum(covid),
                                                        per.covid=100*sum(covid)/sum(deaths),
                                                        exces.ncovid = sum(deaths-covid-fit),
                                                        per.exces.ncovid=100*sum(deaths-covid-fit)/sum(deaths),
                                                        exces.ncovid.lwr = sum(deaths-covid-upr),
                                                        per.exces.ncovid.lwr=100*sum(deaths-covid-upr)/sum(deaths),
                                                        exces.ncovid.upr = sum(deaths-covid-lwr),
                                                        per.exces.ncovid.upr=100*sum(deaths-covid-lwr)/sum(deaths))
temp3$exptd <- paste(round(temp3$exptd), " (", round(temp3$per.expted, digits = 1), ") ", sep = "")
temp3$exptd_bounds <- paste(round(temp3$exptd.lwr)," - ", round(temp3$exptd.upr), 
                            " (", round(temp3$per.expted.lwr, digits = 1)," - ", 
                            round(temp3$per.expted.upr, digits = 1), ")", sep = "")

temp3$exces.ncovid <- paste(round(temp3$exces.ncovid), " (", round(temp3$per.exces.ncovid, digits = 1), ") ", sep = "")
temp3$exces_bounds <- paste(round(temp3$exces.ncovid.lwr)," - ", round(temp3$exces.ncovid.upr), 
                            " (", round(temp3$per.exces.ncovid.lwr, digits = 1)," - ", 
                            round(temp3$per.exces.ncovid.upr, digits = 1), ")", sep = "")

temp3$covid.num <- paste(round(temp3$covid.num), " (", round(temp3$per.covid, digits = 1), ")", sep = "")
temp3$region <- gsub("[.]"," ",temp3$region)
EW <- temp3[temp3$region %in% c("England", "Wales"),]
temp3 <- temp3[!temp3$region %in% c("England", "Wales"),]
temp3 <- rbind(temp3, EW)
regional_tab <- temp3 %>% select('region', 'alldeaths', 'exptd', 'exptd_bounds', 'covid.num', 'exces.ncovid', 'exces_bounds')
colnames(regional_tab) <- c("Region", "Reported all-cause deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
                            "Excess non-COVID", "Excess non-COVID 95% prediction interval (%)")
write.csv(regional_tab, "Table1.csv")

# Age group table
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

Agegrp_temp<-Agegrp_temp %>% mutate(excess.deaths.upr=deaths-covid-lwr, 
                                    excess.deaths.lwr=deaths-covid-upr, 
                                    excess.deaths.expected=deaths-covid-fit) # this temp used for coefficient/multiplier (see below)

temp4<-Agegrp_temp %>% group_by(region) %>% summarise(alldeaths = sum(deaths),
                                                      exptd = sum(fit),
                                                      per.expted=100*sum(fit)/sum(deaths), 
                                                      exptd.upr = sum(upr),
                                                      per.expted.upr=100*sum(upr)/sum(deaths),
                                                      exptd.lwr = sum(lwr),
                                                      per.expted.lwr=100*sum(lwr)/sum(deaths),
                                                      covid.num = sum(covid),
                                                      per.covid=100*sum(covid)/sum(deaths),
                                                      exces.ncovid = sum(deaths-covid-fit),
                                                      per.exces.ncovid=100*sum(deaths-covid-fit)/sum(deaths),
                                                      exces.ncovid.lwr = sum(deaths-covid-upr),
                                                      per.exces.ncovid.lwr=100*sum(deaths-covid-upr)/sum(deaths),
                                                      exces.ncovid.upr = sum(deaths-covid-lwr),
                                                      per.exces.ncovid.upr=100*sum(deaths-covid-lwr)/sum(deaths))
temp4$exptd <- paste(round(temp4$exptd), " (", round(temp4$per.expted, digits = 1), "%) ", sep = "")
temp4$exptd_bounds <- paste(round(temp4$exptd.lwr)," - ", round(temp4$exptd.upr), 
                            " (", round(temp4$per.expted.lwr, digits = 1),"% - ", 
                            round(temp4$per.expted.upr, digits = 1), "%)", sep = "")

temp4$exces.ncovid <- paste(round(temp4$exces.ncovid), " (", round(temp4$per.exces.ncovid, digits = 1), "%) ", sep = "")
temp4$exces_bounds <- paste(round(temp4$exces.ncovid.lwr)," - ", round(temp4$exces.ncovid.upr), 
                            " (", round(temp4$per.exces.ncovid.lwr, digits = 1),"% - ", 
                            round(temp4$per.exces.ncovid.upr, digits = 1), "%)", sep = "")

temp4$covid.num <- paste(round(temp4$covid.num), " (", round(temp4$per.covid, digits = 1), "%)", sep = "")
temp4$region <- gsub("[.]"," ",temp4$region)
temp4$region <- gsub("All", "All:", temp4$region)
temp4$region <- gsub("Female", "Female:", temp4$region)
temp4$region <- gsub("Male", "Male:", temp4$region)
temp4$region <- gsub("45 64", "45-64", temp4$region)
temp4$region <- gsub("65 74", "65-74", temp4$region)
temp4$region <- gsub("75 84", "75-84", temp4$region)
temp4$region <- gsub("85 ", "85+", temp4$region)
agegrp_tab <- temp4 %>% select('region','alldeaths', 'exptd', 'exptd_bounds', 'covid.num', 'exces.ncovid', 'exces_bounds')
colnames(agegrp_tab) <- c("Age group", "Reported all-cause deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
                            "Excess non-COVID", "Excess non-COVID 95% prediction interval (%)")
write.csv(agegrp_tab, "Table2.csv")
