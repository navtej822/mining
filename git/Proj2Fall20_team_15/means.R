library(lubridate)

setwd("/Users/ntb/Desktop/data_mining/datasets_all")
hourly_2006 <- read_table2("/Users/ntb/Desktop/data_mining/datasets_all/hourly_2006.g",
                           col_names = FALSE, locale = locale(), skip = 1)
hourly_2009 <- read_table2("/Users/ntb/Desktop/data_mining/datasets_all/hourly_2009.g",
                           col_names = FALSE, locale = locale(), skip = 1)
hourly_2010 <- read_table2("/Users/ntb/Desktop/data_mining/datasets_all/hourly_2010_mod.g",
                           col_names = FALSE, locale = locale(), skip = 1)

hourly_2006$X20 <- NULL
colnames(hourly_2006) [1:19] <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP","STP","Count_STP","Visib","Count_Visib",
                                  "WDSP","Count_WDSP","MXSDP","Gust","PRCP","SNDP","FRSHIFT")
hourly_2009$X20 <- NULL
colnames(hourly_2009) [1:19] <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP","STP","Count_STP","Visib","Count_Visib",
                                  "WDSP","Count_WDSP","MXSDP","Gust","PRCP","SNDP","FRSHIFT")
hourly_2010$X20 <- NULL
colnames(hourly_2010) [1:19] <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP","STP","Count_STP","Visib","Count_Visib",
                                  "WDSP","Count_WDSP","MXSDP","Gust","PRCP","SNDP","FRSHIFT")



hourly_2006_team15 <- subset(hourly_2006, grepl("200603", yearModa_hr))
hourly_2009_team15 <- subset(hourly_2009, grepl("200903", yearModa_hr))
hourly_2010_team15 <- subset(hourly_2010, grepl("201003", yearModa_hr))


hourly_2006_team15$yearModa_hr<-ymd_h(hourly_2006_team15$yearModa_hr)
hourly_2009_team15$yearModa_hr<-ymd_h(hourly_2009_team15$yearModa_hr)
hourly_2010_team15$yearModa_hr<-ymd_h(hourly_2010_team15$yearModa_hr)


View(hourly_2006_team15)
str(hourly_2006_team15)
View(hourly_2009_team15)
str(hourly_2009_team15)
View(hourly_2010_team15)
str(hourly_2010_team15)


hourly_2006_team15$date<-as.Date(hourly_2006_team15$yearModa_hr)
hourly_2006_team15$time<-format(as.POSIXct(hourly_2006_team15$yearModa_hr),format="%H:%M:%S")
hourly_2009_team15$date<-as.Date(hourly_2009_team15$yearModa_hr)
hourly_2009_team15$time<-format(as.POSIXct(hourly_2009_team15$yearModa_hr),format="%H:%M:%S")
hourly_2010_team15$date<-as.Date(hourly_2010_team15$yearModa_hr)
hourly_2010_team15$time<-format(as.POSIXct(hourly_2010_team15$yearModa_hr),format="%H:%M:%S")

hour2006.pre<-hourly_2006_team15
hour2006.pre$WBAN<-NULL
hour2006.pre$PRCP<-NULL
hour2006.pre$Count_DewP<-NULL
hour2006.pre$SLP<-NULL
hour2006.pre$Count_SLP<-NULL
hour2006.pre$Count_STP<-NULL
hour2006.pre$Visib<-NULL
hour2006.pre$Count_Visib<-NULL
hour2006.pre$Count_WDSP<-NULL
hour2006.pre$FRSHIFT<-NULL
hour2006.pre$SNDP<-NULL
hour2006.pre$Gust<-NULL
hour2006.pre$MXSDP<-NULL
hour2006.pre$yearModa_hr<-NULL
#View(hourly_2006_team15)


hour2009.pre<-hourly_2009_team15
hour2009.pre$WBAN<-NULL
hour2009.pre$PRCP<-NULL
hour2009.pre$Count_DewP<-NULL
hour2009.pre$SLP<-NULL
hour2009.pre$Count_SLP<-NULL
hour2009.pre$Count_STP<-NULL
hour2009.pre$Visib<-NULL
hour2009.pre$Count_Visib<-NULL
hour2009.pre$Count_WDSP<-NULL
hour2009.pre$FRSHIFT<-NULL
hour2009.pre$SNDP<-NULL
hour2009.pre$Gust<-NULL
hour2009.pre$MXSDP<-NULL
hour2009.pre$yearModa_hr<-NULL
#View(hourly_2009_team15)


hour2010.pre<-hourly_2010_team15
hour2010.pre$WBAN<-NULL
hour2010.pre$PRCP<-NULL
hour2010.pre$Count_DewP<-NULL
hour2010.pre$SLP<-NULL
hour2010.pre$Count_SLP<-NULL
hour2010.pre$Count_STP<-NULL
hour2010.pre$Visib<-NULL
hour2010.pre$Count_Visib<-NULL
hour2010.pre$Count_WDSP<-NULL
hour2010.pre$FRSHIFT<-NULL
hour2010.pre$SNDP<-NULL
hour2010.pre$Gust<-NULL
hour2010.pre$MXSDP<-NULL
hour2010.pre$yearModa_hr<-NULL

hour2006.pre <- hour2006.pre %>% replace_with_na(replace = list(Temp = 9999.9 ,DewP = 9999.9, STP = 9999.9, WDSP = 999.9))
hour2009.pre <- hour2009.pre %>% replace_with_na(replace = list(Temp = 9999.9 ,DewP = 9999.9, STP = 9999.9, WDSP = 999.9))
hour2010.pre <- hour2010.pre %>% replace_with_na(replace = list(Temp = 9999.9 ,DewP = 9999.9, STP = 9999.9, WDSP = 999.9))
#View(hourly_2010_team15)
qplot(Temp,date,data=hour2006.pre)+xlim(0,200)
qplot(Temp,date,data=hour2009.pre)+xlim(0,200)
qplot(Temp,date,data=hour2010.pre)+xlim(0,200)
#------------Finding the means of day and month for 2006 March--------------------------

group_date<-group_by(hour2006.pre,date)
mean_2006<-summarise(group_date,avg_temp=mean(Temp,na.rm=TRUE),avg_dewP=mean(DewP,na.rm = TRUE),avg_STP=mean(STP,na.rm = TRUE),
                     avg_WDSP=mean(WDSP,na.rm = TRUE))
View(mean_2006)
month_mean_2006<-summarise(mean_2006,avg_temp=mean(avg_temp,na.rm=TRUE),avg_dewP=mean(avg_dewP,na.rm = TRUE),avg_STP=mean(avg_STP,na.rm = TRUE),
                           avg_WDSP=mean(avg_WDSP,na.rm = TRUE))
month_mean_2006<-cbind(Year_Mon ="2006 March",month_mean_2006)
View(month_mean_2006)

#----------------------------------------------------------------------------------------


#------------Finding the means of day and month for 2009 March--------------------------

group_date<-group_by(hour2009.pre,date)
mean_2009<-summarise(group_date,avg_temp=mean(Temp,na.rm=TRUE),avg_dewP=mean(DewP,na.rm = TRUE),avg_STP=mean(STP,na.rm = TRUE),
                     avg_WDSP=mean(WDSP,na.rm = TRUE))
View(mean_2009)
month_mean_2009<-summarise(mean_2009,avg_temp=mean(avg_temp,na.rm=TRUE),avg_dewP=mean(avg_dewP,na.rm = TRUE),avg_STP=mean(avg_STP,na.rm = TRUE),
                           avg_WDSP=mean(avg_WDSP,na.rm = TRUE))
month_mean_2009<-cbind(Year_Mon ="2009 March",month_mean_2009)
View(month_mean_2009)

#----------------------------------------------------------------------------------------

#------------Finding the means of day and month for 2010 March--------------------------

group_date<-group_by(hour2010.pre,date)
mean_2010<-summarise(group_date,avg_temp=mean(Temp,na.rm=TRUE),avg_dewP=mean(DewP,na.rm = TRUE),avg_STP=mean(STP,na.rm = TRUE),
                     avg_WDSP=mean(WDSP,na.rm = TRUE))
View(mean_2010)
month_mean_2010<-summarise(mean_2010,avg_temp=mean(avg_temp,na.rm=TRUE),avg_dewP=mean(avg_dewP,na.rm = TRUE),avg_STP=mean(avg_STP,na.rm = TRUE),
                           avg_WDSP=mean(avg_WDSP,na.rm = TRUE))
month_mean_2010<-cbind(Year_Mon ="2010 March",month_mean_2010)
View(month_mean_2010)

#----------------------------------------------------------------------------------------


#-----------------------------------merge tables-----------------------------------------

final_mean_6910<-rbind(month_mean_2006,month_mean_2009,month_mean_2010)
View(final_mean_6910)

#-----------------------------------------END---------------------------------------------
