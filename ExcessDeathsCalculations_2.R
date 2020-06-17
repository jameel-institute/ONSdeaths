# This files estimates several local linear model with trigonometric seasonality for 
# different regions and age groups and forecasts for a period of 2 year 
# starting in March 2020

library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(matrixStats)
options(scipen=9999)
library(KFAS)
library(gridExtra)


# read in the file CleanedData.csv and average the the number of deaths in the last week one year with
# the number of deaths in the first week of the subsequent year. This is to account for delays in registration
# which create a through and a spike 

file<-"CleanedDataForEstimation.csv"
data<-read.csv(file,stringsAsFactors = F)
data$Week.ended<-as.Date(data$Week.ended,"%Y-%m-%d")
data$last.week.year<-0
data$year<-format(data$Week.ended,"%Y")
data[(data$Week.number==52 & data$year %in% c(2010,2011,2012,2013,2014,2016,2017,2018,2019)),'last.week.year']<-1

data[data$Week.number==53,'last.week.year']<-1
data$X<-NULL
pos<-which(data$last.week.year ==1)

for (i in pos){
  x1<-data[data$X==i, 3:34]
  x2<-data[data$X==i+1, 3:34]
  x<-(x1+x2)/2
  data[data$X==i, 3:34]<-x
  data[data$X==i+1, 3:34]<-x
}



# create number of death for England as sum of its region
data$England<-data$North.East+
              data$North.West+
              data$Yorkshire.and.The.Humber+
              data$East.Midlands+
              data$West.Midlands+
              data$East+
              data$London+
              data$South.East+
              data$South.West          


data$last.week.year<-NULL
data$year<-NULL
data$Week.number<-NULL




weekly.forecasts<-function(data,var){
  # The forecasting period starts in March 2020
  end1<-ymd("2020-03-01")
  # number of observations forecasted for 2 years
  
  Train<-data[data$Week.ended<end1,] # Data for model estimation
  
  a1 <- matrix(c(0,0),2, 1)
  P1 <- matrix(0, 2, 2)
  P1inf <- diag(2)
  
  # define model as a local linear trend + trigonometric seasonality
  m<-as.formula(paste0(var,'~SSMseasonal(period = 52.18, sea.type = "trigonometric") +
                     SSMtrend(degree = 2, Q = list(NA,0),a1 = a1, P1inf = P1inf)'))
  
  model <- SSModel(m,
                   H = matrix(NA),
                   data=Train)
  
  print("Weekly: estimating")
  fit <- fitSSM(model, inits = c(0,0), method = "BFGS")
  
  print("Weekly: smoothing")
  out <- KFS(fit$model,filtering='state',smoothing=c('state','disturbance','mean'))
  Train$predicted <-as.numeric(signal(out, states = c('season','trend'), filtered=FALSE )$signal)
  
  
  df<-data.frame(Week.ended =seq(as.Date("2010-01-08"),as.Date("2022-03-01"), by="week"))
  
  df$signal<-NA
  df[df$Week.ended < end1,c('signal',var)]<-Train[Train$Week.ended < end1,c('predicted',var)]
  #df[df$Week.ended < end1,var]<-Train[,var]
  
  print("Weekly: forecasting")
  n<-nrow(df[df$Week.ended >= end1,])
  
  newdata<-SSModel(rep(NA,n)~SSMseasonal(period = 52.18, sea.type = "trigonometric")+
                     SSMtrend(degree = 2,Q =as.list(fit$model$Q[1:2]) ),H = fit$model$H)
  

  pred <- predict(fit$model, newdata=newdata,  interval = c( "prediction"), level = 0.95, 
                  states =c('all'), se.fit = FALSE, nsim = 1000, 
                  maxiter = 52, filtered = FALSE)
  
  pred<-as.data.frame(pred)
 
  df$fit<-NA
  df$upr<-NA
  df$lwr<-NA
  df$Week.ended<-as.Date(df$Week.ended)
  df[df$Week.ended>=end1,c('fit','lwr','upr')]<-pred
 
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
    
    print("Cumulative: estimating")
    fit <- fitSSM(model, inits = c(0,0),method = "BFGS")
    print("Cumulative: smoothing")
    out <- KFS(fit$model,filtering='state',smoothing=c('state','disturbance','mean'))   
    
    print("Cumulative: simulating")
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
    
    return(df)
    # the output contains the forecasts fit, the uppler and the following quantile of the simulated distribution 2.5%, 50% and 97.5%
    }




# The routing performing the estimation and the forecating starts here.
# It forecasts all variables in data for two years and saves them in ./forecasts

set.seed(18041306)

for (var in colnames(data[,2:34])){
  print(var)

  df<-weekly.forecasts(data,var)  
  
  # this saves a dataframe containing "Week.ended","var", "signal","fit","upr","lwr", "covid" 
  write.csv(df,paste0("./forecasts/",var,".csv"))

  temp<-cumulative.forecasts(data,var)

  write.csv(temp,paste0("./forecasts/",var,"_cumulative.csv"))
    
}
