# This script cleans up the files provided by the ONS regarding mortality from 
# 2010 to march 2020. The data files can be downloaded from
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
# The data for 2020 is cleaned and updated separately
# Any ONS file for 2020 is fine provided it covers the period up to March 1 2020
# This script just needs to be run once.

# The downloaded file is contained in the subdirectory data


library(tidyverse)
library(readxl)
library(stringr)


# The following function is used for cleaning up the files
#setwd("./data")

s<-function(pos,data){
  data[pos[1],1]<-paste0("All ",data[pos[1],1])
  data[pos[2],1]<-paste0("Male ",data[pos[2],1])
  data[pos[3],1]<-paste0("Female ",data[pos[3],1])
  return (data)
}


# Download the files from ONS website

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2010/publishedweek2010.xls"
download.file(ONS_url, destfile="./data/publishedweek2010.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2011/publishedweek2011.xls"
download.file(ONS_url, destfile="./data/publishedweek2011.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2012/publishedweek2012.xls"
download.file(ONS_url, destfile="./data/publishedweek2012.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2013/publishedweek2013.xls"
download.file(ONS_url, destfile="./data/publishedweek2013.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2014/publishedweek2014.xls"
download.file(ONS_url, destfile="./data/publishedweek2014.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2015/publishedweek2015.xls"
download.file(ONS_url, destfile="./data/publishedweek2015.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2016/publishedweek522016.xls"
download.file(ONS_url, destfile="./data/publishedweek522016.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2017/publishedweek522017.xls"
download.file(ONS_url, destfile="./data/publishedweek522017.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2018/publishedweek522018withupdatedrespiratoryrow.xls"
download.file(ONS_url, destfile="./data/publishedweek522018withupdatedrespiratoryrow.xls", quiet=T)

ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls"
download.file(ONS_url, destfile="./data/publishedweek522019.xls", quiet=T)

# ONS_url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek222020.xlsx"
# file2020<-"./data/publishedweek232020.xlsx"
# download.file(ONS_url, destfile=file2020, quiet=T)

current_date = strsplit(as.character(Sys.Date()), '-')[[1]]
current_year = current_date[1]
current_week = as.numeric(strftime(c(Sys.Date()), format = "%V")) - 2
filename = sprintf("publishedweek%s%s.xlsx", current_week, current_year)
ONS_url = sprintf("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/%s/%s", current_year, filename)
download.file(ONS_url, destfile=paste0("./data/", "newest_results.xlsx"), quiet=T)


files<- list.files("./data")
length(files)
files
files<-files[is.na(str_match(files, ".*.xlsx"))]

group1 <- files[as.numeric(str_extract(files, "201[0-9]"))<2016]
group1

for (file in group1) {
  print(file)
  year<-str_extract(file, "201[0-9]")
  data<-read_xls(paste0("./data/",file),sheet=paste0('Weekly Figures ',year) ,col_names = FALSE)
  data<-data[complete.cases(data),]
  
  r<-c('Under 1 year', '01-14', '15-44','45-64', '65-74','75-84','85+')
  
  for (n in r){
    pos<-which(data[,1] == n)
    data<-s(pos,data)
  }
  data[ data == ":" ] <- NA
  write.table(t(data), file=paste0("./DataCleaned/mortality",year,".csv"), row.names=FALSE, col.names=FALSE, sep=",")
}


group2<-files[!(files %in% group1)]
group2

for (file in group2) {
  print(file)
  year<-str_extract(file, "20[0-9]{2}")
  print(year)
  data<-read_xls(paste0("./data/",file),sheet=paste0('Weekly figures ',year) ,col_names = FALSE)
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
  #data$Week.ended<-as.Date(data$Week.ended, origin = "1899-12-30")
  write.table(t(data), file=paste0("./DataCleaned/mortality",year,".csv"), row.names=FALSE, col.names=FALSE, sep=",")
}


############
data<-read_xlsx("./data/newest_results.xlsx", sheet='Weekly figures 2020', col_names = FALSE)
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

write.table(data, file=paste0("./DataCleaned/mortality2020.csv"), row.names=FALSE, col.names=FALSE, sep=",")


# The 2020 file needs to be cleaned up further

data<-read.csv("./DataCleaned/mortality2020.csv",stringsAsFactors = FALSE)

temp<-read.csv("./DataCleaned/mortality2019.csv",stringsAsFactors = FALSE)
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

write.table(data, file=paste0("./DataCleaned/mortality2020.csv"), row.names=FALSE, sep=",")

# aside: There are some problem in Week.ended in the file for 2011.
# This error is a bit random and may affect different columns. Please check the .csv file
temp<- read.csv("./DataCleaned/mortality2011.csv")
temp[1:16 , "Week.ended"]<-as.numeric(as.Date(temp[1:16 , "Week.ended"],format="%d-%b-%y") -as.Date(0, origin="1899-12-30", tz='UTC'))
write.csv(temp,"./DataCleaned/mortality2011.csv")


# Now the datasets are joined up
var<-colnames(temp)
var<-var[c(1:3,7:37)]
data<-read.csv("./DataCleaned/mortality2010.csv",stringsAsFactors = FALSE)
data$Week.ended<-as.Date(data$Week.ended, origin = "1899-12-30")
data<-data[,var]
files<-list.files("./DataCleaned")
files<-files[files!="mortality2010.csv"]

for (file in files){
  temp<-read.csv(paste0("./DataCleaned/",file),stringsAsFactors = FALSE)
  temp$Week.ended<-as.Date(temp$Week.ended, origin = "1899-12-30")
  print(file)
  data<-bind_rows(data, temp[,var])
}


#data$Week.ended<-abs(data$Week.ended)
#data$Week.ended<-as.Date(data$Week.ended, origin = "1899-12-30")

write.csv(data,"CleanedDataForEstimation.csv") 


