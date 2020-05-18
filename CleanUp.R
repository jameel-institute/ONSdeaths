library(ggplot2)
library(tidyverse)
library(KFAS)
library(readxl)
library(stringr)
setwd("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/data")

s<-function(pos,data){
  data[pos[1],1]<-paste0("All ",data[pos[1],1])
  data[pos[2],1]<-paste0("Male ",data[pos[2],1])
  data[pos[3],1]<-paste0("Female ",data[pos[3],1])
  return (data)
}

files<- list.files()
length(files)
files
files<-files[is.na(str_match(files, ".*.xlsx"))]

group1 <- files[as.numeric(str_extract(files, "201[0-9]"))<2016]
group1

for (file in group1) {
  print(file)
  year<-str_extract(file, "201[0-9]")
  data<-read_xls(file,sheet=paste0('Weekly Figures ',year) ,col_names = FALSE)
  data<-data[complete.cases(data),]

  r<-c('Under 1 year', '01-14', '15-44','45-64', '65-74','75-84','85+')

  for (n in r){
    pos<-which(data[,1] == n)
    data<-s(pos,data)
  }
  data[ data == ":" ] <- NA
  write.table(t(data), file=paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/DataCleaned/mortality",year,".csv"), row.names=FALSE, col.names=FALSE, sep=",")
}


group2<-files[!(files %in% group1)]
group2

for (file in group2) {
  print(file)
  year<-str_extract(file, "20[0-9]{2}")
  print(year)
  data<-read_xls(file,sheet=paste0('Weekly figures ',year) ,col_names = FALSE)
  data<-data[complete.cases(data[,3:ncol(data)]),]
  data[is.na(data[,1]),1]<-data[is.na(data[,1]),2]
  k<-(1:nrow(data))[data$...1=="E12000001"]
  data[(k:(k+9)),1]<-data[(k:(k+9)),2]
  data[,2]<-NULL
  
  
  r<-c('Under 1 year', '01-14', '15-44','45-64', '65-74','75-84','85+')
  
  for (n in r){
    pos<-which(data[,1] == n)
    data<-s(pos,data)
    
  }
  data[data==":"]<-NA
  write.table(t(data), file=paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/DataCleaned/mortality",year,".csv"), row.names=FALSE, col.names=FALSE, sep=",")
}
############
############
data<-read_xlsx('publishedweek182020.xlsx',sheet='Weekly figures 2020' ,col_names = FALSE)
###########
###########
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

write.table(data, file=paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/DataCleaned/mortality2020.csv"), row.names=FALSE, col.names=FALSE, sep=",")


# The 2020 file needs to be cleaned up further

data<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/DataCleaned/mortality2020.csv",stringsAsFactors = FALSE)

temp<-read.csv("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/DataCleaned/mortality2019.csv",stringsAsFactors = FALSE)
colnames(temp)

colnames(data)[(colnames(data) %in% colnames(temp))]
colnames(data)[!(colnames(data) %in% colnames(temp))]


colnames(temp)[!(colnames(temp) %in% colnames(data))]

data$Total.deaths..average.of.corresponding<-data$week.over.the.previous.5.years.1
data$week.over.the.previous.5.years.1<-NULL

data$All.Under.1.year<-data$All..1
data$Male.Under.1.year<-data$Male..1
data$Female.Under.1.year<-data$Female..1
data$All..1<-NULL
data$Male..1<-NULL
data$Female..1<-NULL

data$All.01.14<-data$All.1.4+data$All.5.9+data$All.10.14
data$Male.01.14<-data$Male.1.4+data$Male.5.9+data$Male.10.14
data$Female.01.14<-data$Female.1.4+data$Female.5.9+data$Female.10.14
data$All.1.4<-NULL
data$All.5.9<-NULL
data$All.10.14<-NULL
data$Male.1.4<-NULL
data$Male.5.9<-NULL
data$Male.10.14<-NULL
data$Female.1.4<-NULL
data$Female.5.9<-NULL
data$Female.10.14<-NULL

data$All.15.44<-data$All.15.19+data$All.20.24+data$All.25.29+data$All.30.34+data$All.35.39+
  data$All.40.44                                                                            	                                                   
data$Male.15.44<-data$Male.15.19+data$Male.20.24+data$Male.25.29+data$Male.30.34+data$Male.35.39+
  data$Male.40.44                                                                            	                                                   
data$Female.15.44<-data$Female.15.19+data$Female.20.24+data$Female.25.29+data$Female.30.34+
  data$Female.35.39+data$Female.40.44                                                                           	                                                   
data$All.15.19<-NULL
data$All.20.24<-NULL
data$All.25.29<-NULL
data$All.30.34<-NULL
data$All.35.39<-NULL
data$All.40.44 <-NULL
data$Male.15.19<-NULL
data$Male.20.24<-NULL
data$Male.25.29<-NULL
data$Male.30.34<-NULL
data$Male.35.39<-NULL
data$Male.40.44<-NULL
data$Female.15.19<-NULL
data$Female.20.24<-NULL
data$Female.25.29<-NULL
data$Female.30.34<-NULL
data$Female.35.39<-NULL
data$Female.40.44 <-NULL

data$All.45.64<-data$All.45.49+data$All.50.54+data$All.55.59+data$All.60.64
data$Male.45.64<-data$Male.45.49+data$Male.50.54+data$Male.55.59+data$Male.60.64
data$Female.45.64<-data$Female.45.49+data$Female.50.54+data$Female.55.59+data$Female.60.64
data$All.45.49<-NULL
data$All.50.54<-NULL
data$All.55.59<-NULL
data$All.60.64<-NULL
data$Female.45.49<-NULL
data$Female.50.54<-NULL
data$Female.55.59<-NULL
data$Female.60.64<-NULL
data$Male.45.49<-NULL
data$Male.50.54<-NULL
data$Male.55.59<-NULL
data$Male.60.64<-NULL

data$All.65.74<-data$All.65.69+data$All.70.74
data$Male.65.74<-data$Male.65.69+data$Male.70.74
data$Female.65.74<-data$Female.65.69+data$Female.70.74
data$All.65.69<-NULL
data$All.70.74<-NULL
data$Male.65.69<-NULL
data$Male.70.74<-NULL
data$Female.65.69<-NULL
data$Female.70.74<-NULL

data$All.75.84<-data$All.75.79+data$All.80.84
data$Male.75.84<-data$Male.75.79+data$Male.80.84
data$Female.75.84<-data$Female.75.79+data$Female.80.84
data$All.75.79<-NULL
data$All.80.84<-NULL
data$Male.75.79<-NULL
data$Male.80.84<-NULL
data$Female.75.79<-NULL
data$Female.80.84<-NULL

data$All.85.<-data$All.85.89+data$All.90.
data$Male.85.<-data$Male.85.89+data$Male.90.
data$Female.85.<-data$Female.85.89+data$Female.90.
data$All.85.89<-NULL
data$All.90.<-NULL
data$Male.85.89<-NULL
data$Male.90.<-NULL
data$Female.85.89<-NULL
data$Female.90.<-NULL

data$All.respiratory.diseases..ICD.10.J00.J99..ICD.10.v.2013..IRIS.<-data$Deaths.where.the.underlying.cause.was.respiratory.disease..ICD.10.J00.J99.
data$Deaths.where.the.underlying.cause.was.respiratory.disease..ICD.10.J00.J99.<-NULL

write.table(data, file=paste0("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/DataCleaned/mortality2020.csv"), row.names=FALSE, sep=",")


setwd("/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths//DataCleaned")
data<-read.csv("mortality2010.csv",stringsAsFactors = FALSE)


files<-list.files()
files<-files[files!="mortality2010.csv"]



for (file in files){
  temp<-read.csv(file,stringsAsFactors = FALSE)
  print(file)
  data<-bind_rows(data, temp)
}

#data$Week.ended<-abs(data$Week.ended)
data$Week.ended<-as.Date(data$Week.ended, origin = "1899-12-30")


write.csv(data,"/Users/alessandralochen/Documents/covid-19 hospitalizations/ONS deaths/CleanedData.csv")



