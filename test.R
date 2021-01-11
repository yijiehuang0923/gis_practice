library(readr)
library(tidyverse)
data_district <- read_csv("https://raw.githubusercontent.com/yijiehuang0923/gis_practice/main/od_matrix.csv",
                          locale = locale(encoding = "latin1"),
                          na = "n/a")

data_district1.5_1<-data_district %>%
  filter(Total_distance_km<=1.500)
i<-2
data_district1.5_1<-left_join(data_district1.5_1,summarise(group_by(data_district1.5_1,DestinationID),sum(people_k*Total_distance_km^i)),by='DestinationID')
data_district1.5_1$service=data_district1.5_1$bednum/data_district1.5_1$`sum(people_k * Total_distance_km^i)`
data_district1.5_1<-left_join(data_district1.5_1,summarise(group_by(data_district1.5_1,OriginID),sum(service*Total_distance_km^i)),by='OriginID')
data_district1.5_1$accessibility=data_district1.5_1$`sum(service * Total_distance_km^i)`

data_district1.5_2<-data_district[order(data_district$DestinationID),][0:170,]
data_district1.5_2<-left_join(data_district1.5_2,summarise(group_by(data_district1.5_1,OriginID),mean(accessibility)),by='OriginID')
data_district1.5_2[is.na(data_district1.5_2)]<-0
data_district1.5_2<-rename(data_district1.5_2,'accessibility'='mean(accessibility)')


data_district10_1<-data_district %>%
  filter(Total_distance_km<=10)
a<-1
data_district10_1$sum_distance<-data_district10_1$Total_distance_km
data_district10_1$sum_distance[data_district10_1$sum_distance<=1.5]<-data_district10_1$Total_distance_km^i
data_district10_1$sum_distance[data_district10_1$sum_distance>1.5]<-(data_district10_1$Total_distance_km-1.5)^a+1.5^2
data_district10_1<-left_join(data_district10_1,summarise(group_by(data_district10_1,DestinationID),sum(people_k*sum_distance)),by='DestinationID')
data_district10_1$service=data_district10_1$bednum/data_district10_1$`sum(people_k * sum_distance)`
data_district10_1<-left_join(data_district10_1,summarise(group_by(data_district10_1,OriginID),sum(service * sum_distance)),by='OriginID')
data_district10_1$accessibility=data_district10_1$`sum(service * sum_distance)`

data_district10_2<-data_district[order(data_district$DestinationID),][0:170,]
data_district10_2<-left_join(data_district10_2,summarise(group_by(data_district10_1,OriginID),mean(accessibility)),by='OriginID')
data_district10_2[is.na(data_district10_2)]<-0
data_district10_2<-rename(data_district10_2,'accessibility'='mean(accessibility)')


data_district20_1<-data_district %>%
  filter(Total_distance_km<=20)
b<-0.5
data_district20_1$sum_distance<-data_district20_1$Total_distance_km
data_district20_1$sum_distance<-(data_district20_1$Total_distance_km-10)^b+1.5^2+(10-1.5)^a
data_district20_1$sum_distance[is.nan(data_district20_1$sum_distance)]<-0
data_district20_1$sum_distance[data_district20_1$sum_distance<=1.5]<-data_district20_1$Total_distance_km^i
data_district20_1$sum_distance[data_district20_1$sum_distance>1.5 & data_district20_1$sum_distance<=10]<-(data_district20_1$Total_distance_km-1.5)^a+1.5^2

data_district20_1<-left_join(data_district20_1,summarise(group_by(data_district20_1,DestinationID),sum(people_k*sum_distance)),by='DestinationID')
data_district20_1$service=data_district20_1$bednum/data_district20_1$`sum(people_k * sum_distance)`
data_district20_1<-left_join(data_district20_1,summarise(group_by(data_district20_1,OriginID),sum(service * sum_distance)),by='OriginID')
data_district20_1$accessibility=data_district20_1$`sum(service * sum_distance)`

data_district20_2<-data_district[order(data_district$DestinationID),][0:170,]
data_district20_2<-left_join(data_district20_2,summarise(group_by(data_district20_1,OriginID),mean(accessibility)),by='OriginID')
data_district20_2[is.na(data_district20_2)]<-0
data_district20_2<-rename(data_district20_2,'accessibility'='mean(accessibility)')
