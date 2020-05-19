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
  mdf$variable <- factor(mdf$variable, levels=c("fit","var","var_covid"), #, "signal" # comment out signal for March onwards
                         labels=c(paste("Forecasted 06/03-", format(max(mdf$Week.ended), "%d/%m"), sep = ""),
                                  "Observed all-cause","Registered non-COVID deaths")) # , "Predicted" # take out Predicted for March onwards
  p<-ggplot(mdf, aes(x = Week.ended, y = value, colour = variable))+
    geom_ribbon(aes(ymin = lwr, ymax = upr), colour = NA, fill = "grey70",alpha=0.5)+
    geom_line() + 
    scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%d/%m") + # for March onwards
    #scale_x_date(breaks=unique(mdf$Week.ended),date_labels = "%m/%y", date_breaks = "1 month") + # for full forecast
    theme_bw(base_size = 12, base_family = "Helvetica")+ theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(gsub("[.]"," ",sub(".csv", "",i)))+
    scale_colour_manual(values = c("Black","Red", "Purple")) + #, "Blue" # take out Blue for March onwards
    labs(colour = "Deaths", y ="Deaths", x = "")
  
  return(p)
}

legendgrob <- g_legend(pp("Yorkshire.and.The.Humber.csv") + theme(legend.position = "bottom"))

# plot of England & Wales from March onwards
#pdf("Deaths_EW_Marchonwards.pdf", width = 10, height = 4)
grid.arrange(pp("England.csv") + theme(legend.position = "none"), pp("Wales.csv") +theme(legend.position = "none"),
             bottom = legendgrob, ncol=2) #pdf 7x3
#dev.off()

# plot England regions
pdf("Regional_forecasts_England_fullforecast.pdf", width = 10, height = 7)
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
dev.off()

# plot older age groups (all sexes)
pdf("Deaths_Allolder_Maronwards.pdf", width = 7, height = 5)
grid.arrange(pp("All.85..csv")+theme(legend.position = "none")+ggtitle("All 85+ yrs"),#+ylim(0, 10000),
             pp("All.75.84.csv")+theme(legend.position = "none")+ggtitle("All 75-84 yrs"),#+ylim(0, 10000),
             pp("All.65.74.csv")+theme(legend.position = "none")+ggtitle("All 65-74 yrs"),#+ylim(0, 10000),
             pp("All.45.64.csv")+theme(legend.position = "none")+ggtitle("All 45-64 yrs"),#+ylim(0, 10000),
             bottom = legendgrob, ncol=2) #pdf 7x5
dev.off()

# plot all sex and older ages
pdf("Deaths_AllolderAllsex_fullforecast.pdf", width = 7, height = 10)
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
dev.off()

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
temp3$alldeaths <- format(temp3$alldeaths, big.mark=",")
temp3$exptd <- paste(format(round(temp3$exptd), big.mark=","), " (", 
                     format(round(temp3$per.expted, digits = 1), big.mark=","), "%)", 
                     sep = "")
temp3$exptd_bounds <- paste(format(round(temp3$exptd.lwr), big.mark=","), " -", 
                            format(round(temp3$exptd.upr), big.mark=","), " (", 
                            round(temp3$per.expted.lwr, digits = 1),"% - ", 
                            round(temp3$per.expted.upr, digits = 1), "%)", sep = "")

temp3$exces.ncovid <- paste(format(round(temp3$exces.ncovid), big.mark=","), " (", 
                            round(temp3$per.exces.ncovid, digits = 1), "%)", sep = "")
temp3$exces_bounds <- paste(format(round(temp3$exces.ncovid.lwr), big.mark=","), " - ", 
                            format(round(temp3$exces.ncovid.upr), big.mark=","), " (", 
                            round(temp3$per.exces.ncovid.lwr, digits = 1),"% - ", 
                            round(temp3$per.exces.ncovid.upr, digits = 1), "%)", sep = "")

temp3$covid.num <- paste(format(round(temp3$covid.num), big.mark=","), " (", 
                         round(temp3$per.covid, digits = 1), "%)", sep = "")
temp3$region <- gsub("[.]"," ",temp3$region)
EW <- temp3[temp3$region %in% c("England", "Wales"),]
temp3 <- temp3[!temp3$region %in% c("England", "Wales"),]
temp3 <- rbind(temp3, EW)
regional_tab <- temp3 %>% select('region', 'alldeaths', 'exptd', 'exptd_bounds', 'covid.num', 'exces.ncovid', 'exces_bounds')
colnames(regional_tab) <- c("Region", "Reported all-cause deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
                            "Excess non-COVID", "Excess non-COVID 95% prediction interval (%)")
write.csv(regional_tab, "Table1.csv")

# Table A2: EW excess deaths each week
tab3 <- Regional_temp %>% group_by(region) %>% filter(region %in% c("England", "Wales")) %>% 
  mutate(excess.ncovid = paste(format(round(excess.deaths.expected), big.mark = ","), " (",
                               format(round(excess.deaths.lwr), big.mark = ","), ",",
                               format(round(excess.deaths.upr), big.mark = ","), ")", sep = "")) %>%
  select(Week.ended, region, excess.ncovid)
tab3 <- reshape2::dcast(tab3, region~Week.ended)
write.csv(tab3, "TableA2.csv")

# Table A3: Coefficient multiplier covid to non-covid excess deaths
Regional_temp$coeff <- Regional_temp$excess.deaths.expected/Regional_temp$covid
Regional_temp$coeff_lwr <- Regional_temp$excess.deaths.lwr/Regional_temp$covid
Regional_temp$coeff_upr <- Regional_temp$excess.deaths.upr/Regional_temp$covid
Regional_temp <- Regional_temp[Regional_temp$Week.ended>=as.Date("2020-04-03"),]
temp_coeff <- Regional_temp %>% mutate(coeff_bounds = paste(round(Regional_temp$coeff, digits = 2), " (",
                                                            round(Regional_temp$coeff_lwr, digits = 2), " - ",
                                                            round(Regional_temp$coeff_upr, digits = 2), ")", sep =""))
temp_coeff <- temp_coeff %>% select("Week.ended", "region", "coeff_bounds")
temp_coeff$region <- gsub("[.]"," ",temp3$region)
temp_coeff <- reshape2::dcast(temp_coeff, region~Week.ended)
write.csv(temp_coeff, "TableA2Coeff_Regional.csv")

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
temp4$alldeaths <- format(temp4$alldeaths, big.mark=",")
temp4$exptd <- paste(format(round(temp4$exptd), big.mark=","), " (", 
                     format(round(temp4$per.expted, digits = 1), big.mark=","), "%)", 
                     sep = "")
temp4$exptd_bounds <- paste(format(round(temp4$exptd.lwr), big.mark=","), " -", 
                            format(round(temp4$exptd.upr), big.mark=","), " (", 
                            round(temp4$per.expted.lwr, digits = 1),"% - ", 
                            round(temp4$per.expted.upr, digits = 1), "%)", sep = "")

temp4$exces.ncovid <- paste(format(round(temp4$exces.ncovid), big.mark=","), " (", 
                            round(temp4$per.exces.ncovid, digits = 1), "%)", sep = "")
temp4$exces_bounds <- paste(format(round(temp4$exces.ncovid.lwr), big.mark=","), " - ", 
                            format(round(temp4$exces.ncovid.upr), big.mark=","), " (", 
                            round(temp4$per.exces.ncovid.lwr, digits = 1),"% - ", 
                            round(temp4$per.exces.ncovid.upr, digits = 1), "%)", sep = "")

temp4$covid.num <- paste(format(round(temp4$covid.num), big.mark=","), " (", 
                         round(temp4$per.covid, digits = 1), "%)", sep = "")
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
