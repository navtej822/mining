install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("stats")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("amap")

library(readr)
library(naniar)
library(dplyr)
library(amap)
library(stats)
library(ggplot2)
library(ggfortify)
library(ggmap) 
library(tidyr)
library(gridExtra)

setwd("/Users/ntb/Desktop/data_mining/datasets_all")


# ------------------------ START OF 2006 ----------------------------------------- #

hourly_2006 <- read_table2("/Users/ntb/Desktop/data_mining/datasets_all/hourly_2006.g",
                           col_names = FALSE, locale = locale(), skip = 1)

hourly_2006$X20 <- NULL
colnames(hourly_2006) [1:19] <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP","STP","Count_STP","Visib","Count_Visib",
                                  "WDSP","Count_WDSP","MXSDP","Gust","PRCP","SNDP","FRSHIFT")

# ----------------------------Preprocessing--------------------------------------- #
hourly_2006_team15 <- subset(hourly_2006, grepl("200603", yearModa_hr))
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

#View(hour2006.pre)
#str(hour2006.pre)
#-----------------------replacing missing values with mean-----------------------#
hour2006.pre <- hour2006.pre %>% replace_with_na(replace = list(Temp = 9999.9 ,DewP = 9999.9, STP = 9999.9, WDSP = 999.9))
hour2006.pre$Temp<-hour2006.pre$Temp %>% replace_na(64.07625)
hour2006.pre$DewP<-hour2006.pre$DewP %>% replace_na(45.42776)
hour2006.pre$STP<-hour2006.pre$STP %>% replace_na(978.7317)
hour2006.pre$WDSP<-hour2006.pre$WDSP %>% replace_na(9.139335)

#View(hour2006.pre)

#------------------------grouping by stations-------------------------------------#
final_2006 <- hour2006.pre %>% dplyr::group_by(STN) %>% summarise(AvgTemp = mean(Temp,na.rm = TRUE)
                                                                  ,AvgDewP = mean(DewP,na.rm = TRUE),AvgSTP = mean(STP,na.rm = TRUE),AvgWDSP = mean(WDSP,na.rm = TRUE))


#View(final_2006)

#--------------------------normalising--------------------------------------------#
final_2006[c(2,3,4,5)] <- scale(final_2006[c(2,3,4,5)])

#--------------------------------------------------------------------------------#
#View(final_2006)
#---------------------------------loading stations dataset------------------------#
location_data <- read.csv("/Users/ntb/Desktop/data_mining/stations.csv")

#View(location_data)
stations <- distinct(location_data, StationNumber, .keep_all = TRUE)
#View(stations)

#-------------------------------------wss plot------------------------------------#
wssplot_pear<-function(data,nc=8,seed=15)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(Kmeans(data,centers=i,method="pearson",iter.max = 25)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum ofsquares",main = "2006 March pearson")
}

wssplot_euc<-function(data,nc=8,seed=15)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(Kmeans(data,centers=i,method="euclidean",iter.max = 25)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum ofsquares",main = "2006 March euclidean")
}


wssplot_pear(final_2006)
wssplot_euc(final_2006)


set.seed(15)


#-------------------------------------Kmeans with pearson and euclidean and then plotting---------------------#
KM2_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =2,iter.max = 30, method = "pearson")
KM3_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =3,iter.max = 30, method = "pearson")
KM4_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =4,iter.max = 30, method = "pearson")
KM5_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =5,iter.max = 30, method = "pearson")
KM6_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =6,iter.max = 30, method = "pearson")
KM7_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =7,iter.max = 30, method = "pearson")
KM8_pearson_2006 = Kmeans(final_2006[,2:5], nstart = 25,centers =8,iter.max = 30, method = "pearson")

P2_pearson_2006 <- autoplot(KM2_pearson_2006, geom = "point", frame= TRUE,data = final_2006) + ggtitle("k = 2")
P3_pearson_2006 <- autoplot(KM3_pearson_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 3")
P4_pearson_2006 <- autoplot(KM4_pearson_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 4")
P5_pearson_2006 <- autoplot(KM5_pearson_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 5")
P6_pearson_2006 <- autoplot(KM6_pearson_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 6")
P7_pearson_2006 <- autoplot(KM7_pearson_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 7")
P8_pearson_2006 <- autoplot(KM8_pearson_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 8")


KM2_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 2, method = "euclidean")
KM3_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 3, method = "euclidean")
KM4_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 4, method = "euclidean")
KM5_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 5, method = "euclidean")
KM6_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 6, method = "euclidean")
KM7_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 7, method = "euclidean")
KM8_euclidean_2006 = Kmeans(final_2006[,2:5], nstart = 25, iter.max = 30,centers = 8, method = "euclidean")

p2_euclidean_2006 <- autoplot(KM2_euclidean_2006, geom = "point", frame= TRUE,data = final_2006) + ggtitle("k = 2")
p3_euclidean_2006 <- autoplot(KM3_euclidean_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 3")
p4_euclidean_2006 <- autoplot(KM4_euclidean_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 4")
p5_euclidean_2006 <- autoplot(KM5_euclidean_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 5")
p6_euclidean_2006 <- autoplot(KM6_euclidean_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 6")
p7_euclidean_2006 <- autoplot(KM7_euclidean_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 7")
p8_euclidean_2006 <- autoplot(KM8_euclidean_2006, geom = "point",  frame= TRUE,data = final_2006) + ggtitle("k = 8")


grid.arrange(P2_pearson_2006, P3_pearson_2006, P4_pearson_2006,P5_pearson_2006,P6_pearson_2006,P7_pearson_2006,P8_pearson_2006, nrow = 4)

grid.arrange(p2_euclidean_2006, p3_euclidean_2006, p4_euclidean_2006,p5_euclidean_2006,p6_euclidean_2006,p7_euclidean_2006,p8_euclidean_2006, nrow = 4)


autoplot(KM2_euclidean_2006, final_2006, frame=TRUE)
KM2_euclidean_2006$centers
KM2_euclidean_2006$cluster
KM2_euclidean_2006$size
KM2_euclidean_2006$withinss
str(KM2_euclidean_2006)
KM3_euclidean_2006$size
autoplot(KM2_pearson_2006, final_2006, frame=TRUE)
KM2_pearson_2006$centers
KM2_pearson_2006$cluster
KM2_pearson_2006$size
KM2_pearson_2006$withinss
str(KM2_pearson_2006)

final_2006$cluster <- as.factor(KM2_euclidean_2006$cluster)
final_2006_plot_euclid <- merge.data.frame(final_2006, stations, by.x = "STN", by.y = "StationNumber")
#View(final_2006_plot_euclid)

final_2006$cluster <- as.factor(KM2_pearson_2006$cluster)
final_2006_plot_pearson <- merge.data.frame(final_2006, stations, by.x = "STN", by.y = "StationNumber")
#View(final_2006_plot_pearson)
#---------------------------------plotting clusters on the map------------------------------#
qmplot(Lon, Lat, data = final_2006_plot_euclid, maptype = "toner-hybrid", color = as.factor(cluster))
qmplot(Lon, Lat, data = final_2006_plot_pearson, maptype = "toner-hybrid", color = as.factor(cluster))


# ------------------------ END OF 2006 ----------------------------------------- #




# ********************** START OF 2009 **************************************** #


hourly_2009 <- read_table2("/Users/ntb/Desktop/data_mining/datasets_all/hourly_2009.g",
                           col_names = FALSE, locale = locale(), skip = 1)

hourly_2009$X20 <- NULL
colnames(hourly_2009) [1:19] <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP","STP","Count_STP","Visib","Count_Visib",
                                  "WDSP","Count_WDSP","MXSDP","Gust","PRCP","SNDP","FRSHIFT")
# ----------------------------Preprocessing--------------------------------------- #
hourly_2009_team15 <- subset(hourly_2009, grepl("200903", yearModa_hr))

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

#View(hour2009.pre)
str(hour2009.pre)
#-----------------------replacing missing values with mean-----------------------#
hour2009.pre <- hour2009.pre %>% replace_with_na(replace = list(Temp = 9999.9 ,DewP = 9999.9, STP = 9999.9, WDSP = 999.9))
hour2009.pre$Temp<-hour2009.pre$Temp %>% replace_na(64.34492)
hour2009.pre$DewP<-hour2009.pre$DewP %>% replace_na(43.89203)
hour2009.pre$STP<-hour2009.pre$STP %>% replace_na(978.6333)
hour2009.pre$WDSP<-hour2009.pre$WDSP %>% replace_na(9.208489)

#View(hour2009.pre)

#------------------------grouping by stations-------------------------------------#
final_2009 <- hour2009.pre %>% dplyr::group_by(STN) %>% summarise(AvgTemp = mean(Temp,na.rm = TRUE)
                                                                  ,AvgDewP = mean(DewP,na.rm = TRUE),AvgSTP = mean(STP,na.rm = TRUE),AvgWDSP = mean(WDSP,na.rm = TRUE))


#View(final_2009)

#--------------------------normalising------------------------------------#
final_2009[c(2,3,4,5)] <- scale(final_2009[c(2,3,4,5)])

#--------------------------------------------------------------------------#
#View(final_2009)
#---------------------------------loading stations dataset------------------------#

####location_data <- read.csv("/Users/ntb/Desktop/data_mining/stations.csv")

#View(location_data)
###stations <- distinct(location_data, StationNumber, .keep_all = TRUE)
#View(stations)

#-------------------------------------wss plot------------------------------------#
wssplot_pear<-function(data,nc=8,seed=15)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(Kmeans(data,centers=i,method="pearson",iter.max = 25)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum ofsquares",main = "2009 March pearson")
}

wssplot_euc<-function(data,nc=8,seed=15)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(Kmeans(data,centers=i,method="euclidean",iter.max = 25)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum ofsquares",main = "2009 March euclidean")
}
#final_2009 <- data.frame(final_2009[,-1], row.names = final_2009$STN)

wssplot_pear(final_2009)
wssplot_euc(final_2009)


set.seed(15)


#-------------------------------------Kmeans with pearson and euclidean and then plotting---------------------#
KM2_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =2,iter.max = 30, method = "pearson")
KM3_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =3,iter.max = 30, method = "pearson")
KM4_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =4,iter.max = 30, method = "pearson")
KM5_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =5,iter.max = 30, method = "pearson")
KM6_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =6,iter.max = 30, method = "pearson")
KM7_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =7,iter.max = 30, method = "pearson")
KM8_pearson_2009 = Kmeans(final_2009[,2:5], nstart = 25,centers =8,iter.max = 30, method = "pearson")

P2_pearson_2009 <- autoplot(KM2_pearson_2009, geom = "point", frame= TRUE,data = final_2009) + ggtitle("k = 2")
P3_pearson_2009 <- autoplot(KM3_pearson_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 3")
P4_pearson_2009 <- autoplot(KM4_pearson_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 4")
P5_pearson_2009 <- autoplot(KM5_pearson_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 5")
P6_pearson_2009 <- autoplot(KM6_pearson_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 6")
P7_pearson_2009 <- autoplot(KM7_pearson_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 7")
P8_pearson_2009 <- autoplot(KM8_pearson_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 8")


KM2_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 2, method = "euclidean")
KM3_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 3, method = "euclidean")
KM4_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 4, method = "euclidean")
KM5_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 5, method = "euclidean")
KM6_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 6, method = "euclidean")
KM7_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 7, method = "euclidean")
KM8_euclidean_2009 = Kmeans(final_2009[,2:5], nstart = 25, iter.max = 30,centers = 8, method = "euclidean")

p2_euclidean_2009 <- autoplot(KM2_euclidean_2009, geom = "point", frame= TRUE,data = final_2009) + ggtitle("k = 2")
p3_euclidean_2009 <- autoplot(KM3_euclidean_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 3")
p4_euclidean_2009 <- autoplot(KM4_euclidean_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 4")
p5_euclidean_2009 <- autoplot(KM5_euclidean_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 5")
p6_euclidean_2009 <- autoplot(KM6_euclidean_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 6")
p7_euclidean_2009 <- autoplot(KM7_euclidean_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 7")
p8_euclidean_2009 <- autoplot(KM8_euclidean_2009, geom = "point",  frame= TRUE,data = final_2009) + ggtitle("k = 8")


grid.arrange(P2_pearson_2009, P3_pearson_2009, P4_pearson_2009,P5_pearson_2009,P6_pearson_2009,P7_pearson_2009,P8_pearson_2009, nrow = 4)

grid.arrange(p2_euclidean_2009, p3_euclidean_2009, p4_euclidean_2009,p5_euclidean_2009,p6_euclidean_2009,p7_euclidean_2009,p8_euclidean_2009, nrow = 4)


autoplot(KM2_euclidean_2009, final_2009, frame=TRUE)
KM2_euclidean_2009$centers
KM2_euclidean_2009$cluster
KM2_euclidean_2009$size
KM2_euclidean_2009$withinss
str(KM2_euclidean_2009)

autoplot(KM2_pearson_2009, final_2009, frame=TRUE)
KM2_pearson_2009$centers
KM2_pearson_2009$cluster
KM2_pearson_2009$size
KM2_pearson_2009$withinss
str(KM2_pearson_2009)

final_2009$cluster <- as.factor(KM2_euclidean_2009$cluster)
final_2009_plot_euclid <- merge.data.frame(final_2009, stations, by.x = "STN", by.y = "StationNumber")
#View(final_2009_plot_euclid)

final_2009$cluster <- as.factor(KM2_pearson_2009$cluster)
final_2009_plot_pearson <- merge.data.frame(final_2009, stations, by.x = "STN", by.y = "StationNumber")
#View(final_2009_plot_pearson)
#---------------------------------plotting clusters on the map------------------------------#
qmplot(Lon, Lat, data = final_2009_plot_euclid, maptype = "toner-hybrid", color = as.factor(cluster))
qmplot(Lon, Lat, data = final_2009_plot_pearson, maptype = "toner-hybrid", color = as.factor(cluster))


# ********************** END OF 2009 **************************************** #




# ^^^^^^^^^^^^^^^^^^^^^^^ START OF 2010 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


hourly_2010 <- read_table2("/Users/ntb/Desktop/data_mining/datasets_all/hourly_2010_mod.g",
                           col_names = FALSE, locale = locale(), skip = 1)

hourly_2010$X20 <- NULL
colnames(hourly_2010) [1:19] <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count_DewP","SLP","Count_SLP","STP","Count_STP","Visib","Count_Visib",
                                  "WDSP","Count_WDSP","MXSDP","Gust","PRCP","SNDP","FRSHIFT")

# ----------------------------Preprocessing--------------------------------------- #
hourly_2010_team15 <- subset(hourly_2010, grepl("201003", yearModa_hr))

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

#View(hour2010.pre)
str(hour2010.pre)
#-----------------------replacing missing values with mean-----------------------#

hour2010.pre <- hour2010.pre %>% replace_with_na(replace = list(Temp = 9999.9 ,DewP = 9999.9, STP = 9999.9, WDSP = 999.9))
hour2010.pre$Temp<-hour2010.pre$Temp %>% replace_na(59.30610)
hour2010.pre$DewP<-hour2010.pre$DewP %>% replace_na(41.20328)
hour2010.pre$STP<-hour2010.pre$STP %>% replace_na(977.6431)
hour2010.pre$WDSP<-hour2010.pre$WDSP %>% replace_na(8.486867)

#View(hour2010.pre)

#------------------------grouping by stations-------------------------------------#

final_2010 <- hour2010.pre %>% dplyr::group_by(STN) %>% summarise(AvgTemp = mean(Temp,na.rm = TRUE)
                                                                  ,AvgDewP = mean(DewP,na.rm = TRUE),AvgSTP = mean(STP,na.rm = TRUE),AvgWDSP = mean(WDSP,na.rm = TRUE))


#View(final_2010)

#--------------------------normalising------------------------------------#
final_2010[c(2,3,4,5)] <- scale(final_2010[c(2,3,4,5)])
#--------------------------------------------------------------------------#
#View(final_2010)
#---------------------------------loading stations dataset------------------------#

#####location_data <- read.csv("/Users/ntb/Desktop/data_mining/stations.csv")

#View(location_data)
#####stations <- distinct(location_data, StationNumber, .keep_all = TRUE)
#View(stations)
#-------------------------------------wss plot------------------------------------#

wssplot_pear<-function(data,nc=8,seed=15)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(Kmeans(data,centers=i,method="pearson",iter.max = 25)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum ofsquares",main = "2010 March pearson")
}

wssplot_euc<-function(data,nc=8,seed=15)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(Kmeans(data,centers=i,method="euclidean",iter.max = 25)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum ofsquares",main = "2010 March euclidean")
}
#final_2010 <- data.frame(final_2010[,-1], row.names = final_2010$STN)

wssplot_pear(final_2010)
wssplot_euc(final_2010)
head(final_2010)
set.seed(15)
#-------------------------------------Kmeans with pearson and euclidean and then plotting---------------------#

KM2_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =2,iter.max = 30, method = "pearson")
KM3_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =3,iter.max = 30, method = "pearson")
KM4_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =4,iter.max = 30, method = "pearson")
KM5_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =5,iter.max = 30, method = "pearson")
KM6_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =6,iter.max = 30, method = "pearson")
KM7_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =7,iter.max = 30, method = "pearson")
KM8_pearson_2010 = Kmeans(final_2010[,2:5], nstart = 25,centers =8,iter.max = 30, method = "pearson")

P2_pearson_2010 <- autoplot(KM2_pearson_2010, geom = "point", frame= TRUE,data = final_2010) + ggtitle("k = 2")
P3_pearson_2010 <- autoplot(KM3_pearson_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 3")
P4_pearson_2010 <- autoplot(KM4_pearson_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 4")
P5_pearson_2010 <- autoplot(KM5_pearson_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 5")
P6_pearson_2010 <- autoplot(KM6_pearson_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 6")
P7_pearson_2010 <- autoplot(KM7_pearson_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 7")
P8_pearson_2010 <- autoplot(KM8_pearson_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 8")


KM2_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 2, method = "euclidean")
KM3_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 3, method = "euclidean")
KM4_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 4, method = "euclidean")
KM5_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 5, method = "euclidean")
KM6_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 6, method = "euclidean")
KM7_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 7, method = "euclidean")
KM8_euclidean_2010 = Kmeans(final_2010[,2:5], nstart = 25, iter.max = 30,centers = 8, method = "euclidean")

p2_euclidean_2010 <- autoplot(KM2_euclidean_2010, geom = "point", frame= TRUE,data = final_2010) + ggtitle("k = 2")
p3_euclidean_2010 <- autoplot(KM3_euclidean_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 3")
p4_euclidean_2010 <- autoplot(KM4_euclidean_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 4")
p5_euclidean_2010 <- autoplot(KM5_euclidean_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 5")
p6_euclidean_2010 <- autoplot(KM6_euclidean_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 6")
p7_euclidean_2010 <- autoplot(KM7_euclidean_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 7")
p8_euclidean_2010 <- autoplot(KM8_euclidean_2010, geom = "point",  frame= TRUE,data = final_2010) + ggtitle("k = 8")


grid.arrange(P2_pearson_2010, P3_pearson_2010, P4_pearson_2010,P5_pearson_2010,P6_pearson_2010,P7_pearson_2010,P8_pearson_2010, nrow = 4)

grid.arrange(p2_euclidean_2010, p3_euclidean_2010, p4_euclidean_2010,p5_euclidean_2010,p6_euclidean_2010,p7_euclidean_2010,p8_euclidean_2010, nrow = 4)


autoplot(KM2_euclidean_2010, final_2010, frame=TRUE)
KM2_euclidean_2010$centers
KM2_euclidean_2010$cluster
KM2_euclidean_2010$size
KM2_euclidean_2010$withinss
str(KM2_euclidean_2010)

autoplot(KM2_pearson_2010, final_2010, frame=TRUE)
KM2_pearson_2010$centers
KM2_pearson_2010$cluster
KM2_pearson_2010$size
KM2_pearson_2010$withinss
str(KM2_pearson_2010)

final_2010$cluster <- as.factor(KM2_euclidean_2010$cluster)
final_2010_plot_euclid <- merge.data.frame(final_2010, stations, by.x = "STN", by.y = "StationNumber")
#View(final_2010_plot_euclid)

final_2010$cluster <- as.factor(KM2_pearson_2010$cluster)
final_2010_plot_pearson <- merge.data.frame(final_2010, stations, by.x = "STN", by.y = "StationNumber")
#View(final_2010_plot_pearson)
#---------------------------------plotting clusters on the map------------------------------#

qmplot(Lon, Lat, data = final_2010_plot_euclid, maptype = "toner-hybrid", color = as.factor(cluster))
qmplot(Lon, Lat, data = final_2010_plot_pearson, maptype = "toner-hybrid", color = as.factor(cluster))



# ^^^^^^^^^^^^^^^^^^^^^^^ END OF 2010 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #





# %%%%%%%%%%%%%%%%%%%%% START OF JACCARD COMPARISIONS %%%%%%%%%%%%%%%%%%%%%%%%%% #
library(clusteval)
# SubTask 5) Jaccard simmilarity between two clusters of same year for different metrics

a <- cluster_similarity(final_2006_plot_euclid$cluster, final_2006_plot_pearson$cluster, similarity = c("jaccard"), method = "independence")
b <- cluster_similarity(final_2009_plot_euclid$cluster, final_2009_plot_pearson$cluster, similarity = c("jaccard"), method = "independence")
c <- cluster_similarity(final_2010_plot_euclid$cluster, final_2010_plot_pearson$cluster, similarity = c("jaccard"), method = "independence")

jaccard_sameyear_comparision <- data.frame("A" = c("2006 - Euclidean", "2009 - Euclidean","2010 - Euclidean"),
                "B" = c("2006 - Pearson", "2009 - Pearson","2010 - Pearson"), 
                "Similarity Metric" = c("Jaccard", "Jaccard", "Jaccard"),
                "Cluster Similarity" = c(a,b,c),
                stringsAsFactors = FALSE)


View(jaccard_sameyear_comparision)




# SubTask 6) Jaccard similarity between two clusters of different years and different metrics (2006, 2009)


# ----------> (Y1,Y2) = (2006,2009) - Euclidean

#View(final_2006_plot_euclid)
#View(final_2009_plot_euclid)

final_2006_plot_euclid <- final_2006_plot_euclid %>% select(1, 6)
final_2009_plot_euclid <- final_2009_plot_euclid %>% select(1, 6)
colnames(final_2006_plot_euclid)[1:2] <- c("STN", "cluster_2006")
colnames(final_2009_plot_euclid)[1:2] <- c("STN", "cluster_2009")
#View(final_2006_plot_euclid)
#View(final_2009_plot_euclid)
vector2006_2009_merged_pearson <- merge(final_2006_plot_euclid, final_2009_plot_euclid, by = "STN", all = TRUE)
#View(vector2006_2009_merged_pearson)
vector2006_2009_merged_pearson$cluster_2006 <- as.numeric(as.character(vector2006_2009_merged_pearson$cluster_2006))
vector2006_2009_merged_pearson$cluster_2009 <- as.numeric(as.character(vector2006_2009_merged_pearson$cluster_2009))
#View(vector2006_2009_merged_pearson)
vector2006_2009_merged_pearson[is.na(vector2006_2009_merged_pearson)] = 0
#View(vector2006_2009_merged_pearson)
vector2006_2009_merged_pearson$cluster_2006[vector2006_2009_merged_pearson$cluster_2006 == 0] <- 0
vector2006_2009_merged_pearson$cluster_2009[vector2006_2009_merged_pearson$cluster_2009 == 0] <- 0

cluster_similarity(vector2006_2009_merged_pearson$cluster_2006, vector2006_2009_merged_pearson$cluster_2009, similarity = c("jaccard"), method = "independence")






# (Y1,Y2) = (2006,2009) - Pearson

# View(final_2006_plot_pearson)
# View(final_2009_plot_pearson)

final_2006_plot_pearson <- final_2006_plot_pearson %>% select(1, 6)
final_2009_plot_pearson <- final_2009_plot_pearson %>% select(1, 6)
colnames(final_2006_plot_pearson)[1:2] <- c("STN", "cluster_2006")
colnames(final_2009_plot_pearson)[1:2] <- c("STN", "cluster_2009")
#View(final_2006_plot_pearson)
#View(final_2009_plot_pearson)
vector2006_2009_merged_pearson <- merge(final_2006_plot_pearson, final_2009_plot_pearson, by = "STN", all = TRUE)
# View(vector2006_2009_merged_pearson)
vector2006_2009_merged_pearson$cluster_2006 <- as.numeric(as.character(vector2006_2009_merged_pearson$cluster_2006))
vector2006_2009_merged_pearson$cluster_2009 <- as.numeric(as.character(vector2006_2009_merged_pearson$cluster_2009))
#View(vector2006_2009_merged_pearson)
vector2006_2009_merged_pearson[is.na(vector2006_2009_merged_pearson)] = 0
#View(vector2006_2009_merged_pearson)
vector2006_2009_merged_pearson$cluster_2006[vector2006_2009_merged_pearson$cluster_2006 == 0] <- 0
vector2006_2009_merged_pearson$cluster_2009[vector2006_2009_merged_pearson$cluster_2009 == 0] <- 0

cluster_similarity(vector2006_2009_merged_pearson$cluster_2006, vector2006_2009_merged_pearson$cluster_2009, similarity = c("jaccard"), method = "independence")


        
# (Y1, Y2, Y3) - (2006,2009,2010) - Euclidean
final_2010_plot_euclid <- final_2010_plot_euclid %>% select(1, 6)

colnames(final_2010_plot_euclid)[1:2] <- c("STN", "cluster_2010")

vector2006and2010_euclid <- merge(final_2006_plot_euclid, final_2010_plot_euclid, by = "STN", all = TRUE)
vector2006and2010_euclid$cluster_2006 <- as.numeric(as.character(vector2006and2010_euclid$cluster_2006))
vector2006and2010_euclid$cluster_2010 <- as.numeric(as.character(vector2006and2010_euclid$cluster_2010))

vector2006and2010_euclid[is.na(vector2006and2010_euclid)] = 0
vector2006and2010_euclid$cluster_2010[vector2006and2010_euclid$cluster_2006 == 0] <- 0
vector2006and2010_euclid$cluster_2006[vector2006and2010_euclid$cluster_2010 == 0] <- 0

vector2009and2010_euclid <- merge(final_2009_plot_euclid, final_2010_plot_euclid, by = "STN", all = TRUE)
vector2009and2010_euclid$cluster_2009 <- as.numeric(as.character(vector2009and2010_euclid$cluster_2009))
vector2009and2010_euclid$cluster_2010 <- as.numeric(as.character(vector2009and2010_euclid$cluster_2010))

vector2009and2010_euclid[is.na(vector2009and2010_euclid)] = 0
vector2009and2010_euclid$cluster_2010[vector2009and2010_euclid$cluster_2009 == 0] <- 0
vector2009and2010_euclid$cluster_2009[vector2009and2010_euclid$cluster_2010 == 0] <- 0

c1 = cluster_similarity(vector2006_2009_merged_pearson$cluster_2006, vector2006_2009_merged_pearson$cluster_2009, similarity = c("jaccard"), method = "independence")
c2 = cluster_similarity(vector2006and2010_euclid$cluster_2006, vector2006and2010_euclid$cluster_2010, similarity = c("jaccard"), method = "independence")
c3 = cluster_similarity(vector2009and2010_euclid$cluster_2009, vector2009and2010_euclid$cluster_2010, similarity = c("jaccard"), method = "independence")

similarity_euclid = (c1 + c2 + c3) / 3
similarity_euclid



# (Y1, Y2, Y3) - (2006,2009,2010) - Pearson

final_2010_plot_pearson <- final_2010_plot_pearson %>% select(1, 6)

colnames(final_2010_plot_pearson)[1:2] <- c("STN", "cluster_2010")

vector2006and2010_pearson <- merge(final_2006_plot_pearson, final_2010_plot_pearson, by = "STN", all = TRUE)
vector2006and2010_pearson$cluster_2006 <- as.numeric(as.character(vector2006and2010_pearson$cluster_2006))
vector2006and2010_pearson$cluster_2010 <- as.numeric(as.character(vector2006and2010_pearson$cluster_2010))

vector2006and2010_pearson[is.na(vector2006and2010_pearson)] = 0
vector2006and2010_pearson$cluster_2010[vector2006and2010_pearson$cluster_2006 == 0] <- 0
vector2006and2010_pearson$cluster_2006[vector2006and2010_pearson$cluster_2010 == 0] <- 0

vector2009and2010_pearson <- merge(final_2009_plot_pearson, final_2010_plot_pearson, by = "STN", all = TRUE)
vector2009and2010_pearson$cluster_2009 <- as.numeric(as.character(vector2009and2010_pearson$cluster_2009))
vector2009and2010_pearson$cluster_2010 <- as.numeric(as.character(vector2009and2010_pearson$cluster_2010))

vector2009and2010_pearson[is.na(vector2009and2010_pearson)] = 0
vector2009and2010_pearson$cluster_2010[vector2009and2010_pearson$cluster_2009 == 0] <- 0
vector2009and2010_pearson$cluster_2009[vector2009and2010_pearson$cluster_2010 == 0] <- 0

c1 = cluster_similarity(vector2006_2009_merged_pearson$cluster_2006, vector2006_2009_merged_pearson$cluster_2009, similarity = c("jaccard"), method = "independence")
c2 = cluster_similarity(vector2006and2010_pearson$cluster_2006, vector2006and2010_pearson$cluster_2010, similarity = c("jaccard"), method = "independence")
c3 = cluster_similarity(vector2009and2010_pearson$cluster_2009, vector2009and2010_pearson$cluster_2010, similarity = c("jaccard"), method = "independence")

similarity_pearson = (c1 + c2 + c3) / 3
similarity_pearson


# %%%%%%%%%%%%%%%%%%%%% END OF JACCARD COMPARISIONS %%%%%%%%%%%%%%%%%%%%%%%%%% #

