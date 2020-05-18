# This is to calculate excess deaths
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(matrixStats)
options(scipen=9999)
library(KFAS)
library(gridExtra)

file<-"/Volumes/GoogleDrive/My Drive/Covid-19/Mortality/CleanedData.csv"
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


temp<-data[,c( "Week.number",                                                                          
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

for (var in colnames(temp[,3:36])){
  Train<-data[data$Week.ended<end1,]
  a1 <- matrix(c(0,0),2, 1)
  P1 <- matrix(0, 2, 2)
  P1inf <- diag(2)
  
  m<-as.formula(paste0(var,'~SSMseasonal(period = 52.18, sea.type = "trigonometric") +
                     SSMtrend(degree = 2, Q = list(NA,0),a1 = a1, P1inf = P1inf)'))
                
  model <- SSModel(m,H = matrix(NA),
                   data=Train)
  print("Estimation model with local linear trend and trigonometric seasonality")
  fit <- fitSSM(model, inits = c(0,0),method = "BFGS")
  
  print("Smoothing")
  out <- KFS(fit$model,filtering='state',smoothing=c('state','disturbance','mean'))
  Train$predicted <-as.numeric(signal(out, states = c('season','trend'), filtered=FALSE )$signal)
  
  df<-data[,c('Week.ended',var)]
  df$signal<-NA
  df[df$Week.ended < end1,'signal']<-Train$predicted
  
  
  n<-nrow(data[data$Week.ended>=end1,])
  
  v<-var(residuals(out,type="response"))
  newdata<-SSModel(rep(NA,n)~SSMseasonal(period = 52.18, sea.type = "trigonometric")+
                     SSMtrend(degree = 2,Q =as.list(fit$model$Q[1:2]) ),H = fit$model$H)
  
  pred <- predict(fit$model, newdata=newdata,  interval = c( "prediction"), level = 0.95, 
                  states =c('all'), se.fit = FALSE, nsim = 1000, 
                  maxiter = 52, filtered = TRUE)
  pred<-as.data.frame(pred)
  df$fit<-NA
  df$upr<-NA
  df$lwr<-NA
  df[df$Week.ended>=end1,c('fit','lwr','upr')]<-pred
  b<-as.Date("2019-07-01")
  print(ggplot(data=df[df$Week.ended>=b,])+aes(x=Week.ended,y=fit)+geom_line()+
          geom_line(aes(x=Week.ended,y=signal),colour='Blue')+
          geom_line(aes(x=Week.ended,y=upr))+
          geom_line(aes(x=Week.ended,y=lwr))+
          geom_line(aes(x=Week.ended,y=.data[[var]],colour='Red'))+
          ggtitle(paste(var))+theme_bw(base_size = 12, base_family = "Helvetica")+
          theme(legend.position="none")+ylab("")+xlab(""))
  #df<-tail(df,n)
  df<-df[df$Week.ended>=b,]
  
  #df$fit<-exp(df$fit)*exp(v/2)
  #df$lwr<-exp(df$lwr)*exp(v/2)
  #df$upr<-exp(df$upr)*exp(v/2)
  write.csv(df,paste0("/Volumes/GoogleDrive/My Drive/Covid-19/Mortality/forecasts/",var,".csv"))
  print(var)
}


# # Now we make the pictures and does not need to be run
# files<- list.files('/Volumes/GoogleDrive/My Drive/Covid-19/Mortality/forecasts/')
# 
# setwd("/Volumes/GoogleDrive/My Drive/Covid-19/Mortality/forecasts/")
# for (i in files ){
#   data<-read.csv(i,stringsAsFactors = F)
#   data$Week.ended<-as.Date(data$Week.ended)
#   tiff(paste0('/Volumes/GoogleDrive/My Drive/Covid-19/Mortality/pictures/',sub(".csv", "",i),".tiff"), units="in", width=5, height=5, res=300)
#   
#   print(ggplot(data)+aes(x=Week.ended,y=fit)+geom_line()+
#           geom_line(aes(x=Week.ended,y=signal),colour='Blue')+
#           geom_line(aes(x=Week.ended,y=upr))+
#           geom_line(aes(x=Week.ended,y=lwr))+
#           geom_line(aes(x=Week.ended,y=.data[[sub(".csv", "",i)]],colour='Red'))+
#           ggtitle(sub(".csv", "",i))+theme_bw(base_size = 12, base_family = "Helvetica")+
#           theme(legend.position="none")+ylab("")+xlab(""))
#   
#   dev.off()
# }



