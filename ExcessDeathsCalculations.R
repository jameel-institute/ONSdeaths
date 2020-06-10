# This is to calculate excess deaths
# It just does the forecast for two years
# 
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(matrixStats)
options(scipen=9999)
library(KFAS)
library(gridExtra)


file<-"/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/CleanedData.csv"
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
#start<-decimal_date(ymd("2010-01-08"))
end1<-ymd("2020-03-01")
#end<-decimal_date(end1)

#decimal_date(ymd("2020-03-01"))

n<-length(data$Week.ended[data$Week.ended>=end1])
# 
data$respiratory<-rowSums(data[,c('ICD.10.v.2010',
                                  'All.respiratory.diseases..ICD.10.J00.J99...ICD.10.v.2010',
                                  'All.respiratory.diseases..ICD.10.J00.J99..ICD.10.v.2013..IRIS.',
                                  'ICD.10.v.2010..NCHS.')], na.rm=T)
data$England<-data$North.East+
  data$North.West+
  data$Yorkshire.and.The.Humber+
  data$East.Midlands+
  data$West.Midlands+
  data$East+
  data$London+
  data$South.East+
  data$South.West          


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


weekly.forecasts<-function(data,var){
  end1<-ymd("2020-03-01") # start of forecasting period
  #n<-length(data$Week.ended[data$Week.ended>=end1]) # number of observations forecasted
  # number of observations forecasted for 2 years
  Train<-data[data$Week.ended<end1,] # Estimate the model only before March 2020
  
  a1 <- matrix(c(0,0),2, 1)
  P1 <- matrix(0, 2, 2)
  P1inf <- diag(2)
  
  # define model as a local linear trend + trigonometric seasonality
  m<-as.formula(paste0(var,'~SSMseasonal(period = 52.18, sea.type = "trigonometric") +
                     SSMtrend(degree = 2, Q = list(NA,0),a1 = a1, P1inf = P1inf)'))
  
  model <- SSModel(m,
                   H = matrix(NA),
                   data=Train)
  
  print("Estimation model with local linear trend and trigonometric seasonality")
  fit <- fitSSM(model, inits = c(0,0), method = "BFGS")
  
  print("Smoothing")
  out <- KFS(fit$model,filtering='state',smoothing=c('state','disturbance','mean'))
  Train$predicted <-as.numeric(signal(out, states = c('season','trend'), filtered=FALSE )$signal)
  
  #df<-data[,c('Week.ended',var)]
  
  df<-data.frame(Week.ended =seq(as.Date("2010-01-08"),as.Date("2022-03-01"), by="week"))
  
  df$signal<-NA
  df[df$Week.ended < end1,c('signal',var)]<-Train[Train$Week.ended < end1,c('predicted',var)]
  #df[df$Week.ended < end1,var]<-Train[,var]
  print("Forecasting")
  n<-nrow(df[df$Week.ended >= end1,])
  
  newdata<-SSModel(rep(NA,n)~SSMseasonal(period = 52.18, sea.type = "trigonometric")+
                     SSMtrend(degree = 2,Q =as.list(fit$model$Q[1:2]) ),H = fit$model$H)
  

  pred <- predict(fit$model, newdata=newdata,  interval = c( "prediction"), level = 0.95, 
                  states =c('all'), se.fit = FALSE, nsim = 1000, 
                  maxiter = 52, filtered = TRUE)
  
  pred<-as.data.frame(pred)
 
  
  
  df$fit<-NA
  df$upr<-NA
  df$lwr<-NA
  df$Week.ended<-as.Date(df$Week.ended)
  df[df$Week.ended>=end1,c('fit','lwr','upr')]<-pred
 
  #b<-as.Date("2019-07-01")
  return(df)
  # the output is Week.ended,  signal (prediction over the estimation period), the number of deaths recoded, 
  # fit (the forecast for 2 year ) , upr, lwr the (95% forecast intervals)
}



cumulative.forecasts<-function(data,var){
    end1<-as.Date("2020-03-01") # start of forecasting period
    
    Train<-data[data$Week.ended < end1,] # Estimate the model only before March 2020
   
    df<-data.frame(Week.ended = seq(as.Date("2010-01-08"),as.Date("2022-03-01"), by="week"))
   
    a1 <- matrix(c(0,0),2, 1)
    P1 <- matrix(0, 2, 2)
    P1inf <- diag(2)
   
    n<-length(df[df$Week.ended>=end1,])  # this is a forecast for 2 years from March 2020
    
    y<-c(Train[,var], rep(NA,n)) # variable to forecast. There are 104 NA corresponding to the forecasts
    
    model <- SSModel( y~SSMseasonal(period = 52.18, sea.type = "trigonometric") +
                        SSMtrend(degree = 2, Q = list(NA,0),a1 = a1, P1inf = P1inf), H = matrix(NA))
    
    print("Estimating")
    fit <- fitSSM(model, inits = c(0,0),method = "BFGS")
    print("Smoothing")
    out <- KFS(fit$model,filtering='state',smoothing=c('state','disturbance','mean'))   
    
    print("Simulating")
    M <- SSModel( y~SSMseasonal(period = 52.18, sea.type = "trigonometric") +
                    SSMtrend(degree = 2, Q =as.list(fit$model$Q[1:2])), H = fit$model$H)
    
    # The estimates above are used to estimate the signal from the time series using 1000 simulation
    # The simulation use the method of Durbin and Koopman. (2002), A simple and efficient simulation 
    # smoother for state space time series analysis, Biometrika, Volume 89, Issue 3
    
    sim<-simulateSSM(M, type =  "signals", 
                     filtered = FALSE, nsim = 1000, antithetics = FALSE, conditional = TRUE)                   
    
    sim<-as.data.frame(sim[,1,])
    
   
    sim<-sim[(nrow(sim)-n+1):nrow(sim),]
    
    temp<-cumsum(sim[,1:(ncol(sim))])
    
    temp<-as.matrix(temp)
    
    probs <- c(0.025, 0.5, 0.975) # these are the quantiles considered.
    temp1<-as.data.frame(rowQuantiles(temp,probs = probs,na.rm = FALSE,type = 7L,drop = TRUE))
   
    df<-data.frame(Week.ended=df[df$Week.ended >= end1,])
    
    df<-cbind(df,temp1)

    df$fit<-rowMeans(temp)
    colnames(df) <- c("Week.ended", "lwr", "50%", "upr", "fit")
    return(df)
    # the output contains the forecasts fit, the uppler and the following quantile of the simulated distribution 2.5%, 50% and 97.5%
    }





#temp2<-weekly.forecasts(data,"London")  
# ggplot(data=temp2[temp2$Week.ended>=ymd("2020-03-01"),])+aes(x=Week.ended,y=fit)+geom_line()+
#  geom_line(aes(x=Week.ended,y=signal),colour='Blue')+
#  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "Gray",alpha=0.3)+
#  geom_line(aes(x=Week.ended,y=.data[var],colour='Red'))+
#  ggtitle(var)+theme_bw(base_size = 12, base_family = "Helvetica")+
#  theme(legend.position="none")+ylab("")+xlab("")


#temp<-cumulative.forecasts(data,"London") 
# ggplot(data=temp)+aes(x=Week.ended,y=fit)+geom_line()+
#    geom_line(aes(x=Week.ended,y=deaths),colour="red")+
#    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "Gray",alpha=0.3)+
#    ggtitle(var)+theme_bw(base_size = 12, base_family = "Helvetica")+
#    theme(legend.position="none")+ylab("")+xlab("")

for (var in colnames(data[,3:35])){
  print(var)

  df<-weekly.forecasts(data,var)  
  
  # this saves a dataframe containing "Week.ended","var", "signal","fit","upr","lwr", "covid" 
  write.csv(df,paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",var,".csv"))

  temp<-cumulative.forecasts(data,var)
  
  write.csv(temp,paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",var,"_cumulative.csv"))
    
}



# Here how to use the saved data to construct confidence intervals
#
extract.week<-function(var){
  forecasts.week<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",var,".csv"),stringsAsFactors = F)
  forecasts.week$Week.ended<-as.Date(forecasts.week$Week.ended)
  forecasts.week<-forecasts.week[,c("Week.ended", "signal","fit","upr","lwr")]
  data<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/CleanedData.csv",stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  if (var=="England"){
    data$England<-rowSums(data[,c("North.East","North.West","Yorkshire.and.The.Humber","East.Midlands",
                                  "West.Midlands", "East","London","South.East",
                                  "South.West")])
  }
  temp<-data[,c("Week.ended",var)]
  forecasts.week<-merge(temp,  forecasts.week,by='Week.ended',all.y=T)
  forecasts.week<-forecasts.week[1:nrow(temp),]
  temp<-NULL
  
  covid<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv")
  covid$Week.ended<-as.Date(covid$Week.ended, origin = "1899-12-30")
  #covid<-rename(covid, Total.deaths..all.ages = Deaths.involving.COVID.19..all.ages1)
  data$respiratory<-NULL
  covid$England<-rowSums(covid[,c("North.East","North.West","Yorkshire.and.The.Humber","East.Midlands",
                                  "West.Midlands", "East","London","South.East",
                                  "South.West")])
  temp<-covid[,c("Week.ended",var),]
  colnames(temp)<-c("Week.ended","Covid")
  forecasts.week<-merge(temp,  forecasts.week, by='Week.ended',all.y=T)
  return(forecasts.week)
}


extract.cumulative<-function(var){
  forecasts.c<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",
                               var,"_cumulative.csv"),
                        stringsAsFactors = F)
  forecasts.c$Week.ended<-as.Date(forecasts.c$Week.ended)
  forecasts.c<-forecasts.c[,c("Week.ended","lwr","X50.","upr","fit")]
 
  colnames(forecasts.c)<-c("Week.ended","lwr","50%","upr","fit")
  #forecasts.c<-forecasts.c[forecasts.c$Week.ended>=as.Date("2020-03-01"), ]
  
  data<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/CleanedData.csv",stringsAsFactors = F)
  data$Week.ended<-as.Date(data$Week.ended)
  if (var=="England"){
    data$England<-rowSums(data[,c("North.East","North.West","Yorkshire.and.The.Humber","East.Midlands",
                                                     "West.Midlands", "East","London","South.East",
                                                     "South.West")])
    }
  temp<-data[data$Week.ended>=as.Date("2020-03-01"),c("Week.ended",var)]
 
  temp[,var]<-cumsum(temp[,var])
  
  temp$Week.ended<-as.Date(temp$Week.ended)
  forecasts.c<-merge(temp,  forecasts.c, by='Week.ended',all.x=T)
  forecasts.c<-forecasts.c[1:nrow(temp),]
  temp<-NULL
  forecasts.c$signal<-NULL
  
  
  covid<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/ONSCovid/Covid_Registration.csv")
  covid$Week.ended<-as.Date(covid$Week.ended, origin = "1899-12-30")
  #covid<-rename(covid, Total.deaths..all.ages = Deaths.involving.COVID.19..all.ages1)
  data$respiratory<-NULL
  covid$England<-rowSums(covid[,c("North.East","North.West","Yorkshire.and.The.Humber","East.Midlands",
                                  "West.Midlands", "East","London","South.East",
                                  "South.West")])
  temp<-covid[,c("Week.ended",var),]
  
  colnames(temp)<-c("Week.ended","Covid")
  temp$Covid<-cumsum(temp$Covid)
  temp$Week.ended<-as.Date(temp$Week.ended)
  forecasts.c<-merge(temp,  forecasts.c, by='Week.ended',all.y=T)
  return(forecasts.c)
}

### England plot of (A) expected, observed and non-covid excess, (B) cumulative deaths and (C) Excess
# var<-"England"
# temp1<-extract.week(var)
# temp2<-extract.cumulative(var)
# 
# grid.arrange(ggplot(data=temp1[temp1$Week.ended>=ymd("2020-03-01"),])+aes(x=Week.ended,y=fit)+geom_line()+
#                geom_line(aes(x=Week.ended,y=.data[[var]]-Covid),colour='Purple')+
#                geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "Gray",alpha=0.3)+
#                geom_line(aes(x=Week.ended,y=.data[[var]],colour='Red'))+
#                ggtitle(var)+theme_bw(base_size = 12, base_family = "Helvetica")+
#                theme(legend.position="none")+ylab("")+xlab(""),
#              ggplot(data=temp2)+aes(x=Week.ended,y=.data[[var]]-Covid)+ geom_line(colour='Purple')+
#                geom_line(aes(x=Week.ended,y=fit),colour='Black')+
#                geom_line(aes(x=Week.ended,y=.data[[var]]),colour='Red')+
#                geom_ribbon(aes(ymax = `97.5%`, ymin =`2.5%`), fill = "Gray",alpha=0.3)+
#                ggtitle("cumulative deaths")+theme_bw(base_size = 12, base_family = "Helvetica")+
#                theme(legend.position="none")+ylab("")+xlab(""),
#              ggplot(data=temp2)+aes(x=Week.ended,y=Covid)+ geom_line(colour='DarkGreen')+
#                geom_line(aes(x=Week.ended,y=.data[[var]]-Covid-fit),colour='Purple')+
#                geom_line(aes(x=Week.ended,y=.data[[var]]-fit),colour='Blue')+
#                geom_ribbon(aes(ymin = .data[[var]]-Covid-`97.5%`, ymax =.data[[var]]-Covid-`2.5%`), fill = "Gray",alpha=0.3)+
#                ggtitle("Excess deaths")+theme_bw(base_size = 12, base_family = "Helvetica")+
#                theme(legend.position="none")+ylab("")+xlab(""),ncol=3)

for (var in colnames(data[,3:35])){
temp1<-extract.week(var)
temp2<-extract.cumulative(var)

grid.arrange(ggplot(data=temp1[temp1$Week.ended>=ymd("2020-03-01"),])+aes(x=Week.ended,y=fit)+geom_line()+
                     geom_line(aes(x=Week.ended,y=.data[[var]]-Covid),colour='Purple')+
                     geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "Gray",alpha=0.3)+
                     geom_line(aes(x=Week.ended,y=.data[[var]],colour='Red'))+
                     ggtitle(var)+theme_bw(base_size = 12, base_family = "Helvetica")+
                     theme(legend.position="none")+ylab("")+xlab(""),
                   ggplot(data=temp2)+aes(x=Week.ended,y=.data[[var]]-Covid)+ geom_line(colour='Purple')+
                     geom_line(aes(x=Week.ended,y=fit),colour='Black')+
                     geom_line(aes(x=Week.ended,y=.data[[var]]),colour='Red')+
                     geom_ribbon(aes(ymax = `97.5%`, ymin =`2.5%`), fill = "Gray",alpha=0.3)+
                     ggtitle("Cumulative deaths")+theme_bw(base_size = 12, base_family = "Helvetica")+
                     theme(legend.position="none")+ylab("")+xlab(""),
                   ggplot(data=temp2)+aes(x=Week.ended,y=Covid)+ geom_line(colour='Purple')+
                     geom_line(aes(x=Week.ended,y=.data[[var]]-Covid-fit),colour='Black')+
                     geom_line(aes(x=Week.ended,y=.data[[var]]-fit),colour='Red')+
                     geom_ribbon(aes(ymin = .data[[var]]-Covid-`97.5%`, ymax =.data[[var]]-Covid-`2.5%`), fill = "Gray",alpha=0.3)+
                     ggtitle("Excess deaths cumulative")+theme_bw(base_size = 12, base_family = "Helvetica")+
                     theme(legend.position="none")+ylab("")+xlab(""),ncol=3)
}

# since the weekly deaths and the cumulative deaths are forecasted a bit differently, 
# they may not be exactly the same. The difference between the number of cumulative 
# deaths however are very small being at most 6000 in two years (giving a difference of 0.04%)
for (var in colnames(data[,3:35])){
  forecasts.week<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",var,".csv"),stringsAsFactors = F)
  forecasts.c<-read.csv(paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/forecasts/",var,"_cumulative.csv"),stringsAsFactors = F)
  forecasts.week<-forecasts.week[forecasts.week$Week.ended>=as.Date("2020-03-01"),c("Week.ended",'fit'),]
  forecasts.c<-forecasts.c[forecasts.c$Week.ended>=as.Date("2020-03-01"),c("Week.ended",'fit'),]
  
  forecasts.c$c<-cumsum(forecasts.week$fit)  
  forecasts.c$Week.ended<-as.Date(forecasts.c$Week.ended)
  print(grid.arrange(ggplot(forecasts.c) +aes(x=Week.ended,y=(fit-c)/fit)+geom_line()+ggtitle(var),
               ggplot(forecasts.c) +aes(x=Week.ended,y=(fit-c))+geom_line(),ncol=2))
}
  
  
  
  