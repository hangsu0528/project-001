
print("Zhaohui Wang")
print(1461347)
print("zwang98@ucsc.edu")

#install package pdlyr
install.packages("dplyr")
library(dplyr)
#install library foreign to load data
library(foreign)
#Quesiton1 
#load data 
df.ex <- read.dta(
   file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
) 
#Question2
#filter the last month to 2013 and print the number of observations 
filter(df.ex,year==2013)
print(sum(with(df.ex,year==2013)))
July <- sum(with(df.ex,year==2013&month==7))
August <- sum(with(df.ex,year==2013&month==8))
Sept <-sum(with(df.ex,year==2013&month==9))
Summer <- sum(July+August+Sept)
print(Summer)
#Question3 
#use arrange to set year and month to ascending 
df.ex.3a <-arrange(df.ex, year, month)
#Question4
#select the colomn that from year to age
df.ex.4a <- select(df.ex.3a,year:age)
df.ex.4b <- select(df.ex.3a,year,month,starts_with("id"))
#to print distinct set of values in the orginal df.ex
print(unique(df.ex$state))
#Question5
#create a vector and return the standard score
stndz <-function(x){
  (x-mean(x,na.rm=T))/sd(x,na.rm=T)
}
#creat a vector and teturn the feature scaled value
nrmlz <-function(x) (x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm=T))
#creat a dataframe to calculate the stndz and nrmlz of the rw
df.ex.5a<- 
  df.ex %>% mutate(
       rw.stndz= stndz(rw),
         rw.nrmlz=nrmlz(rw))
#creat a dataframe 
df.ex.5b <- df.ex %>% 
  group_by(year, month) %>%
  mutate(
                   
                   rw_stndz=stndz(rw),
                   rw_nrmlz=nrmlz(rw),
                   count=n()
                  )

#Question6

df.ex.6<-
  df.ex %>% 
  group_by(year,month,state)%>%
  summarise(
  rw_min=min(rw,na.rm = T),
  rw_1stQnt=quantile(rw,na.rm = T,0.25),
  rw_mean.art =mean(rw,na.rm = T),
  rw_3rdQnt=quantile(rw,na.rm = T,0.75),
  rw_max=max(rw,na.rm = T),
  rw_median=median(rw,na.rm=T),
  count=n()
                        )%>%
  select(state,starts_with("rw_"),count)
#print where year month and state has the highest mean value
print(df.ex.6 %>% ungroup() %>% arrange(desc(rw_mean.art)) %>%
        select(year,month,state) %>% head(1))
 #Question 7
df.ex$state.char <-as.character(df.ex$state)

df.ex.7a <-
  df.ex %>% arrange(year,month,desc(state.char))

