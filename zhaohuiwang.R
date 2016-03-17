---
  title: "zhaohuiwangfinal"
output: html_document
---
  
  ###first download the data 
  

library(nycflights13)
library(dplyr)
library(ggplot2)
library(RSQLite)
flights_sqlite <- tbl(nycflights13_sqlite(), "flights")
flights_sqlite


##a)weather factors


weatherinfo<- flights %>% filter(dep_delay > 300) %>% left_join(weather, by = "origin", copy = TRUE) %>% mutate(canceled = is.na(arr_time))
weatherinfo1 <- weatherinfo %>% group_by(canceled) %>% summarise(mean_delay = mean(dep_delay)) %>% collect()
weatherinfo1 <- as.data.frame(weatherinfo1)
weatherinfo1
graph <- ggplot(weatherinfo1, aes(x= factor(canceled),y=mean_delay)) + geom_bar(fill="green", stat = "identity",width = 0.1)
graph
weatherinfo2<-as.data.frame(weatherinfo)
graph1<- ggplot(weatherinfo2,aes(x=factor(canceled),y=dep_delay))+geom_point(fill="green",stat="identity")


####from the mean of the departure delay we can see that weather is not a reason tha the flights cancelled.



##b)time of day,day of week,and time of year,and any other aspect of year


year_bar <- group_by(flights_sqlite, year) %>% summarise(count = mean(dep_delay))
df <- group_by(flights_sqlite, month) %>% summarise(count = mean(dep_delay))
df <- as.data.frame(df)
p <- ggplot(df, aes(x= factor(month),y=count)) + geom_bar(stat="identity", fill = "green")
p
df <- group_by(flights_sqlite, day) %>% summarise(count = mean(dep_delay))
df <- as.data.frame(df)
p <- ggplot(df, aes(x= day,y=count)) + geom_line()
p


####In months 6 , 7 and 12 the planes have the most departure delays, and months 9,10 and 11 have the least dep_delays. And also the departure delays in different days are different.

##c) plane destination 


destination <- group_by(flights_sqlite, dest) %>%  mutate(canceled = is.na(arr_time)) %>% summarise(count = mean(dep_delay), count2 = n(), count3 = sum(canceled)) 
destination <- as.data.frame(destination)
graph5 <- ggplot(destination, aes(x= dest,y=count)) + geom_bar(stat="identity", fill = "green") + theme(axis.text.x=element_text(angle = 90))
graph5


####from the graph we can see that most of the destinations are busy except for LEX and PSP. This results might due to the inefficient of the data. And CFE and IUL are the two most busy destinations. 

##d)characteristics of the plane 

chara<- flights_sqlite %>% left_join(planes, by = "tailnum", copy = TRUE)

chara1 <- group_by(chara, carrier) %>% summarise(count = mean(dep_delay))
chara1<- as.data.frame(chara1)
graph7<- ggplot(chara1, aes(x= carrier,y=count)) + geom_bar(stat="identity", fill = "green") + theme(axis.text.x=element_text(angle = 90))
graph7


####from the graph we can see that carriers AS HA US has the lowest  possibility to delay .


planeeng <- group_by(chara, engine) %>% summarise(count = mean(dep_delay))
planeeng <- as.data.frame(planeeng)
graph8 <- ggplot(planeeng, aes(x= engine,y=count)) + geom_bar(stat="identity", fill = "green") + theme(axis.text.x=element_text(angle = 90))
graph8


#### from the graph we can see the plane has 4 cycle engine has the worest possiblity to delay .

##So  we should predict that a high porbabiliy departure delays and planes cancellations by carrier AS HA US in 2013 when destination is LEX and in months 6 , 7 and 12. 