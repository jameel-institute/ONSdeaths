library(ggplot2)
library(tidyverse)
library(KFAS)
library(readxl)
library(stringr)
library(dplyr)
setwd("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/data")

s<-function(pos,data){
  data[pos[1],1]<-paste0("All ",data[pos[1],1])
  data[pos[2],1]<-paste0("Male ",data[pos[2],1])
  data[pos[3],1]<-paste0("Female ",data[pos[3],1])
  return (data)
}


# registrations 
###################
###################
data<-read_xlsx('publishedweek182020.xlsx',sheet='Covid-19 - Weekly registrations' ,col_names = FALSE,range='A5:BC86')
###################
###################
data<-data[complete.cases(data[,3:8]),]
data[is.na(data[,1]),1]<-data[is.na(data[,1]),2]
k<-(1:nrow(data))[data$...1=="E12000001"]
data[(k:(k+9)),1]<-data[(k:(k+9)),2]
data[,2]<-NULL


r<-c("<1", "1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",                                                                                
     "60-64","65-69","70-74", "75-79", "80-84", "85-89","90+"   )

for (n in r){
  pos<-which(data[,1] == n)
  data<-s(pos,data)
}
data[data==":"]<-NA
data<-as.data.frame(t(data))
data<-data[complete.cases(data),]

write.table(data, file=paste0("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/ONSCovid/Covid_Registration.csv"), row.names=FALSE, col.names=FALSE, sep=",")


# The 2020 file needs to be cleaned up further

data<-read.csv("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/ONSCovid/Covid_Registration.csv",stringsAsFactors = FALSE)


df <- data %>% mutate(
  All.Under.1.year= All..1,
  Male.Under.1.year=Male..1,
  Female.Under.1.year=Female..1,
  All.01.14=All.1.4+All.5.9+All.10.14,
  Male.01.14=Male.1.4+Male.5.9+Male.10.14,
  Female.01.14=Female.1.4+Female.5.9+Female.10.14,
  All.15.44=All.15.19+All.20.24+All.25.29+All.30.34+All.35.39+All.40.44,                                                                            	                                                   
  Male.15.44=Male.15.19+Male.20.24+Male.25.29+Male.30.34+Male.35.39+Male.40.44,                                                                          	                                                   
  Female.15.44=Female.15.19+Female.20.24+Female.25.29+Female.30.34+Female.35.39+Female.40.44,
  All.45.64=All.45.49+All.50.54+All.55.59+All.60.64,
  Male.45.64=Male.45.49+Male.50.54+Male.55.59+Male.60.64,
  Female.45.64=Female.45.49+Female.50.54+Female.55.59+Female.60.64,
  All.65.74=All.65.69+All.70.74,
  Male.65.74=Male.65.69+Male.70.74,
  Female.65.74=Female.65.69+Female.70.74,
  All.75.84=All.75.79+All.80.84,
  Male.75.84=Male.75.79+Male.80.84,
  Female.75.84=Female.75.79+Female.80.84,
  All.85.=All.85.89+All.90.,
  Male.85.=Male.85.89+Male.90.,
  Female.85.=Female.85.89+Female.90.
)

df<-df[,c(1:3,64:94)]

write.table(df, file=paste0("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/ONSCovid/Covid_Registration.csv"), row.names=FALSE, sep=",")


### Occurrences
##################
################### 
data<-read_xlsx('publishedweek172020.xlsx',sheet='Covid-19 - Weekly occurrences' ,col_names = FALSE,range='A5:BC86')
###################
###################
data<-data[complete.cases(data[,3:8]),]
data[is.na(data[,1]),1]<-data[is.na(data[,1]),2]
k<-(1:nrow(data))[data$...1=="E12000001"]
data[(k:(k+9)),1]<-data[(k:(k+9)),2]
data[,2]<-NULL


r<-c("<1", "1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",                                                                                
     "60-64","65-69","70-74", "75-79", "80-84", "85-89","90+"   )

for (n in r){
  pos<-which(data[,1] == n)
  data<-s(pos,data)
}
data[data==":"]<-NA
data<-as.data.frame(t(data))
data<-data[complete.cases(data),]

write.table(data, file=paste0("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/ONSCovid/Covid_Occurrences.csv"), row.names=FALSE, col.names=FALSE, sep=",")


# The 2020 file needs to be cleaned up further

data<-read.csv("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/ONSCovid/Covid_Occurrences.csv",stringsAsFactors = FALSE)

data<-data[1:(nrow(data)-1),]

df <- data %>% mutate(
  All.Under.1.year= All..1,
  Male.Under.1.year=Male..1,
  Female.Under.1.year=Female..1,
  All.01.14=All.1.4+All.5.9+All.10.14,
  Male.01.14=Male.1.4+Male.5.9+Male.10.14,
  Female.01.14=Female.1.4+Female.5.9+Female.10.14,
  All.15.44=All.15.19+All.20.24+All.25.29+All.30.34+All.35.39+All.40.44,                                                                            	                                                   
  Male.15.44=Male.15.19+Male.20.24+Male.25.29+Male.30.34+Male.35.39+Male.40.44,                                                                          	                                                   
  Female.15.44=Female.15.19+Female.20.24+Female.25.29+Female.30.34+Female.35.39+Female.40.44,
  All.45.64=All.45.49+All.50.54+All.55.59+All.60.64,
  Male.45.64=Male.45.49+Male.50.54+Male.55.59+Male.60.64,
  Female.45.64=Female.45.49+Female.50.54+Female.55.59+Female.60.64,
  All.65.74=All.65.69+All.70.74,
  Male.65.74=Male.65.69+Male.70.74,
  Female.65.74=Female.65.69+Female.70.74,
  All.75.84=All.75.79+All.80.84,
  Male.75.84=Male.75.79+Male.80.84,
  Female.75.84=Female.75.79+Female.80.84,
  All.85.=All.85.89+All.90.,
  Male.85.=Male.85.89+Male.90.,
  Female.85.=Female.85.89+Female.90.
)

df<-df[,c(1:3,64:94)]

write.table(df, file=paste0("/Volumes/GoogleDrive/My Drive/Covid-19/mortality/ONSCovid/Covid_Occurrences.csv"), row.names=FALSE, sep=",")


