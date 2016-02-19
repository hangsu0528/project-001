#question 0 
firstname<- "Zhaohui"
lastname<-"Wang"
print(
  paste(
    firstname,
    lastname
  )
)
studentID<-"1461347"
print(studentID)
#Question 1
#install package dplyr
library(dplyr)
#load files
df.flights<- read.csv("/Users/babe/Desktop/flights.csv",stringsAsFactors = TRUE)
df.planes<-read.csv("/Users/babe/Desktop/planes.csv",stringsAsFactors = TRUE)
df.weather<-read.csv("/Users/babe/Desktop/weather.csv",stringsAsFactors = TRUE)
df.airports<-read.csv("/Users/babe/Desktop/weather.csv",stringsAsFactors = TRUE)
#Question 2
#set all data as dae 
as.Date(df.flights$date,"%m/%d/%y %H%M%S")
as.Date(df.weather$date,"%m%d%y")
as.Date(df.airports$date,"%m%d%y")
#Question 3
#extract flights that went to city of SF and Oakland CA
flights.2a<-subset(df.flights,dest=="SFO"|dest=="OAK")
nrow(flights.2a)
#extract flights that delayed by an hour or more
flights.2b<-subset(df.flights,dep_delay>=60)
nrow(flights.2b)
#extract flights in wich the arrive delay was more than twice the departure delay 
flights.2c<-subset(df.flights,arr_delay>=2*dep_delay)
nrow(flights.2c)
#Question 4
#three different ways to select delay variables
?dplyr::select
select(df.flights,starts_with("arr"))
select(df.flights,ends_with("delay"))
select(df.flights,contains("delay"))
#Question 5
#top five departure delay flights
flights.5a<-df.flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)
#top 5 caught up flights
flights.5b<-df.flights%>%mutate(
  catchuptime=(dep_delay-arr_delay)
)%>%arrange(desc(catchuptime))%>%head(5)
#Question 6
#add new colomn
flights<-mutate(df.flights,
                   mph=dist/(time/60),
                   delta=dep_delay-arr_delay)
View(flights)
#print the top five speed 
flights.6a<-flights%>%select(mph)%>%arrange(desc(mph))%>%head(5)     
print(flights.6a)
#print top 5 catchup planes . check if it matches anwser 5b. 
flights.6b<-flights%>%select(delta)%>%arrange(desc(delta))%>%head(5)
print(flights.6b)
print(flights.5b)
#I am not sure what does it means by lost the most time 
#I assume it means that the flights that take most of the time flying 
flights.6c<-flights%>%select(time)%>%arrange(desc(time))%>%head(1)
print(flights.6c)
#Question 7
#summary info
flights.7a<- flights %>% group_by(carrier) %>%
  summarise (
    cancelled = sum(cancelled),
    total_flights = n(),
    percent_cancelled= (cancelled/total_flights),
    min = min(delta, na.rm = T),
    quantile1 = quantile(delta, .25, na.rm = T),
    quantile2 = quantile(delta, .75, na.rm = T),
    mean = mean(delta, na.rm = T),
    median = median(delta, na.rm = T),
    quantile90 = quantile(delta, .90, na.rm = T),
    max = max(delta, na.rm = T)
  )
#print the summary with the worst prcent cancelled 
flights.7a1<-print(
  flights.7a%>%arrange(desc(percent_cancelled))
)
#explain 
day_delay <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(
        flights,
        !is.na(dep_delay)
        ),
      date),
    delay = mean(dep_delay),
    n = n()
    ),
  n > 10
  )
View(day_delay)
cat("the code above select flights without missing departure delay and group by date, then 
    calculate the mean departure date and number on each date. And In the end discard the rowsum
    that have more than 10 flights")
#rewrite using %>%
day_delay1<- dplyr::filter(flights,!is.na(dep_delay))%>% group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n = n(),
    n>10)
#Question 8
?dplyr::lag
day_delay2<- day_delay%>%mutate(diff =delay-lag(delay)) %>%arrange(desc(diff))
print(day_delay2%>%select(diff)%>%head(5))
#Question 9
#create table 
dest_delay<-flights %>% group_by(dest) %>%
  summarise (
    mean = mean(arr_delay, na.rm = T),
    number_flights=n()
  )
#select data
View(df.airports)
select(df.airports,contains("iata"))
df.airports1<-df.airports%>%select(dest = iata, name = airport , city, state, lat, long)
df.9a <- df.airports %>% left_join(dest_delay, by="dest")
df.9a %>% arrange(desc(avg_arr_delay)) %>% head(5)

df.9b<- df.airports %>% inner_join(dest_delay, by="dest")
print('Not match. They are different.')

df.9c<- df.airports %>% right_join(dest_delay, by="dest")
print('There are 116 observations. No NA appear in avg_arr_delay')

df.9d<- df.airports %>% full_join(dest_delay, by="dest")
print('There are 3378 observations. There are 3262 NA\'s in avg_arr_delay')
print('number of rows of both tables are not the same.')
#Question 10 
hourly_delay <- df.flights %>% filter(!is.na(dep_delay)) %>% group_by(date,hour) %>%
  summarise(delay = mean(dep_delay), n = n())
hourly_delay %>% full_join(weather) %>% group_by(conditions) %>% 
  summarise(max_delay = max(delay, na.rm=T)) %>% arrange(desc(max_delay))
#Question 11
#parta 
df<-data.frame(treatment=c("a","b"),subject1=c(3,4),subject2=c(5,6))
df
library(tidyr)
df %>% gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)
#part b
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df
df %>% spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)
#part c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
df %>% separate(demo, into = c('sex','age','state') , sep = '_')
#part d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df
df <- df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df